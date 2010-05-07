module Data.XDR.Parser
    ( parseString
    , parseFile
    , parseImportSpecification
    , ParseError
    ) where

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import System.Path

import Text.Parsec hiding (ParseError)
import Text.Parsec.ByteString hiding (Parser)
import Text.Parsec.Token (makeTokenParser, GenLanguageDef (..))
import qualified Text.Parsec.Token as T
import Text.Parsec.Expr

import Data.XDR.AST
import Data.XDR.PathUtils

----------------------------------------------------------------
-- Lexer
l = makeTokenParser $
    LanguageDef { commentStart = "/*"
                , commentEnd = "*/"
                , commentLine = "//"
                , nestedComments = True
                , identStart = letter
                , identLetter = alphaNum <|> char '_'
                , opStart = oneOf "+-/*="
                , opLetter = oneOf "+-/*="
                , reservedNames = tokens
                , reservedOpNames = ["+", "-", "/", "*", "="]
                , caseSensitive = True
                }
    where
      tokens = [ "bool", "case", "const", "default", "double", "quadruple", "enum"
               , "float", "hyper", "opaque", "string", "struct", "switch", "typedef"
               , "union", "unsigned", "void", "int", "import"
               ]

angles = T.angles l
braces = T.braces l
colon = T.colon l
comma = T.comma l
commaSep = T.commaSep l
commaSep1 = T.commaSep1 l
identifier = T.identifier l
integer = T.integer l
parens = T.parens l
reserved = T.reserved l
reservedOp = T.reservedOp l
semi = T.semi l
string = T.stringLiteral l
squares = T.squares l
whiteSpace = T.whiteSpace l

----------------------------------------------------------------

-- FIXME: add location detail
data ParseError = ParseError String

instance Show ParseError where
    show (ParseError str) = str

parseString :: [(String, Integer)] -> ByteString -> String -> Either [ParseError] Specification
parseString defines txt source p =
    case runParser xdrParser (initContext defines) source txt of
      Left err -> Left [ParseError . show $ err]
      Right spec -> Right spec

parseFile :: [(String, Integer)] -> AbsFile -> IO (Either [ParseError] Specification)
parseFile defines path = do
  input <- B.readFile path'
  return $ parseString defines input path'
    where
      path' = getPathString path

data ImportSpec = ImportSpec [RelFile] [Definition]

-- FIXME: using ExceptionT IO would simplify this
parseImportSpecification :: [(String, Integer)] -> [AbsDir] -> AbsFile -> IO (Either [ParseError] ImportSpecification)
parseImportSpecification defines includes path = do
  txt <- B.readFile path
  er <- runParser importSpec (initContext defines) path txt
  case er of
    Left errs -> Left errs
    Right (ImportSpec is ds) -> do
      ei <- foldM parseImport is
      case ei of
        Left errs -> return $ Left errs
        Right map -> return $ ImportSpecification map (Specification ds)
      
  where
    parseImport :: String -> Either [ParseError] (Map AbsFile ImportSpecification) -> 
                   IO (Either [ParseError] (AbsFile, ImportSpecification))
    parseImport _ (Left errs) = return $ Left errs
    parseImport im (Right m) = do
      mim <- pathLookup includes im
      case mim of
        Nothing -> return $ Left [ParseError "could not find import '" ++ getPathString im ++ "'"]
        Just im' -> do
          result <- parseImportSpecification defines includes im'
          case result of
            Left errs -> return $ Left errs
            Right ispec -> return $ Right (M.insert im' ispec)
      
data Context = Context { constTable :: Map String Integer
                       , nextEnum :: Integer
                       }

initContext :: [(String, Integer)] -> Context
initContext defines = Context (M.fromList defines) 0

type Parser = GenParser Char Context

----------------------------------------------------------------

discard :: Parser a -> Parser ()
discard = (>> return ())

intConstant :: Parser Integer
intConstant = ((identifier >>= check) <|> integer) <?> "constant"
    where
      check name = getState >>= \ctxt ->
        case M.lookup name (constTable ctxt) of
          Nothing -> unexpected $ "undefined constant reference: " ++ name
          Just x -> pure x

constExpr :: Parser Integer
constExpr = buildExpressionParser table term
    where
      table = [ [ prefix "-" negate
                , prefix "+" id
                ]
              , [ binary "*" (*) AssocLeft
                , binary "/" div AssocLeft
                ]
              , [ binary "+" (+) AssocLeft
                , binary "-" (-) AssocLeft
                ]
              ]

      prefix name fun = Prefix (mkOp name fun)
      binary name fun assoc = Infix (mkOp name fun) assoc
      mkOp name fun = reservedOp name *> pure fun

      term = parens constExpr <|> intConstant 

constant :: Parser Constant
constant = ConstLit <$> constExpr

simpleType :: String -> Type -> Parser Type
simpleType keyword t = const t <$> try (reserved keyword)

typeSpec :: Parser Type
typeSpec = choice [ try (reserved "unsigned" *> (simpleType "int" TUInt <|> simpleType "hyper" TUHyper))
                  , simpleType "int" TInt
                  , simpleType "hyper" THyper
                  , simpleType "float" TFloat
                  , simpleType "double" TDouble
                  , simpleType "quadruple" TQuadruple
                  , simpleType "bool" TBool
                  , try enumTypeSpec
                  , try structTypeSpec
                  , try unionTypeSpec
                  , TTypedef <$> identifier
                  ] <?> "type"

enumTypeSpec :: Parser Type
enumTypeSpec = TEnum <$> (reserved "enum" *> enumDetail)

enumDetail :: Parser EnumDetail
enumDetail = setNextEnum 0 *> braces body
    where
      body = EnumDetail <$> commaSep pair
      pair = ((,) <$> identifier <*> optionMaybe (reservedOp "=" *> constant)) >>= uncurry mkElem

      setNextEnum n = do
        ctxt <- getState
        setState $ ctxt { nextEnum = n }

      incNextEnum = do
        ctxt <- getState
        let v = nextEnum ctxt
        setState $ ctxt { nextEnum = succ v }
        return v

      mkElem n Nothing = incNextEnum >>= insertConst n
      mkElem n (Just (ConstLit v)) = (setNextEnum $ v + 1) *> insertConst n v

      insertConst n v = do
        ctxt <- getState
        setState $ ctxt { constTable = M.insert n v (constTable ctxt) }
        pure (n, ConstLit v)

structTypeSpec :: Parser Type
structTypeSpec = TStruct <$> (reserved "struct" *> structDetail)

structDetail :: Parser StructDetail
structDetail = braces body
    where body = StructDetail <$> many1 (declaration <* semi)

unionTypeSpec :: Parser Type
unionTypeSpec = TUnion <$> (reserved "union" *> unionDetail)

infixl 4 <*-*>
(<*-*>) :: (Applicative f) => f (a -> b -> c) -> f (a, b) -> f c
(<*-*>) fn fab = uncurry <$> fn <*> fab

unionDetail :: Parser UnionDetail
unionDetail = UnionDetail <$> switch <*-*> braces ((,) <$> caseStatements <*> deflt)
    where
      switch         = reserved "switch" *> parens declaration
      caseStatements = many1 caseStatement
      caseStatement  = (,) <$> (reserved "case" *> constant <* colon) <*> declaration <* semi
      deflt          = optionMaybe (reserved "default" *> colon *> declaration <* semi)

declaration :: Parser Decl
declaration =
    choice [ pure DeclVoid <* reserved "void"
           , try (mkString <$> (reserved "string" *> identifier) <*> angles (optionMaybe constant))
           , (reserved "opaque" *> identifier) >>= mkOpaque
           , typeSpec >>= mkBasicOrPointer
           ] <?> "declaration"
    where
      mkBasicOrPointer :: Type -> Parser Decl
      mkBasicOrPointer t = choice [ mkPointer t <$> (reservedOp "*" *> identifier)
                                  , identifier >>= mkBasic t
                                  ]

      mkBasic :: Type -> String -> Parser Decl
      mkBasic t n = choice [ mkArray t n <$> squares constant
                           , mkVarArray t n <$> angles (optionMaybe constant)
                           , pure (mkSimple t n)
                           ]

      mkOpaque :: String -> Parser Decl
      mkOpaque n = choice [ mkFixedOpaque n <$> squares constant
                          , mkVarOpaque n <$> angles (optionMaybe constant)
                          ]

      mkSimple t n    = Decl n $ DeclSimple t
      mkArray t n     = Decl n . DeclArray t
      mkVarArray t n  = Decl n . DeclVarArray t
      mkFixedOpaque n = Decl n . DeclOpaque
      mkVarOpaque n   = Decl n . DeclVarOpaque
      mkString n      = Decl n . DeclString
      mkPointer t n   = Decl n $ DeclPointer t

constantDef :: Parser ConstantDef
constantDef = ((,) <$> (reserved "const" *> identifier <* reservedOp "=") <*> constant) >>= uncurry mkConst
    where
      mkConst name c@(ConstLit v) = do
        ctxt <- getState
        let table = constTable ctxt

        -- maybe the constant has already been defined ?  We only
        -- complain if they're trying to give it a different value.
        case M.lookup name table of
          Nothing -> insert name v table ctxt
          Just v' -> if v /= v'
                     then fail . concat $ [ "multiple, differing, definitions for "
                                          , name
                                          , " ("
                                          , show v
                                          , " != "
                                          , show v'
                                          , ")"
                                          ]
                     else insert name v table ctxt

        pure $ ConstantDef name c

      insert name v table ctxt = setState $ ctxt { constTable = M.insert name v table }

typeDef :: Parser Typedef
typeDef = choice [ mkSimple <$> (reserved "typedef" *> declaration)
                 , mkEnum   <$> (reserved "enum" *> identifier) <*> enumDetail
                 , mkStruct <$> (reserved "struct" *> identifier) <*> structDetail
                 , mkUnion  <$> (reserved "union" *> identifier) <*> unionDetail
                 ] <?> "typedef"
    where
      mkSimple (Decl n di) = Typedef n $ DefSimple di
      mkSimple _           = error "internal error" -- FIXME: get rid of this
      mkEnum n             = Typedef n . DefEnum
      mkStruct n           = Typedef n . DefStruct
      mkUnion n            = Typedef n . DefUnion

definition :: Parser Definition
definition = ((DefConstant <$> constantDef) <|> (DefTypedef <$> typeDef)) <* semi

importStatement :: Parser RelFile
importStatement = reserved "import" *> string <* semi

importSpec :: Parser ImportSpec
importSpec = ImportSpec <$> (whiteSpace *> many importStatement) <*> (many definition <* eof)

xdrParser :: Parser Specification
xdrParser = Specification <$> (whiteSpace *> many definition <* eof)

----------------------------------------------------------------