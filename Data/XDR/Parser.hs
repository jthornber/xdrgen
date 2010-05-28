module Data.XDR.Parser
    ( LanguageOptions (..)
    , parseString
    , parseFile
    , ParseError
    ) where

import Control.Applicative hiding (many, (<|>))
import Control.Arrow
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable hiding (concat)
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

-- | Lexer
l :: (Monad m) => T.GenTokenParser ByteString Context m
l = makeTokenParser
    LanguageDef { commentStart = "/*"
                , commentEnd = "*/"
                , commentLine = "//"
                , nestedComments = True
                , identStart = letter
                , identLetter = alphaNum <|> char '_'
                , opStart = oneOf "+-/*=."
                , opLetter = oneOf "+-/*=."
                , reservedNames = tokens
                , reservedOpNames = ["+", "-", "/", "*", "=", "."]
                , caseSensitive = True
                }
    where
      tokens = [ "bool", "case", "const", "default", "double", "quadruple", "enum"
               , "float", "hyper", "opaque", "string", "struct", "switch", "typedef"
               , "union", "unsigned", "void", "int", "import", "module"
               ]

angles, braces, parens, squares :: (Monad m) => Parser m a -> Parser m a
angles = T.angles l
braces = T.braces l
parens = T.parens l
squares = T.squares l

colon, comma, semi :: (Monad m) => Parser m String
colon = T.colon l
comma = T.comma l
semi = T.semi l

commaSep, commaSep1 :: (Monad m) => Parser m a -> Parser m [a]
commaSep = T.commaSep l
commaSep1 = T.commaSep1 l

identifier :: (Monad m) => Parser m String
identifier = T.identifier l

integer :: (Monad m) => Parser m Integer
integer = T.integer l

reserved, reservedOp :: (Monad m) => String -> Parser m ()
reserved = T.reserved l
reservedOp = T.reservedOp l

stringLiteral :: (Monad m) => Parser m String
stringLiteral = T.stringLiteral l

whiteSpace :: (Monad m) => Parser m ()
whiteSpace = T.whiteSpace l



-- | Extensions to the basic XDR language can be turned on with these options.
data LanguageOptions = 
  -- | Allows the caller to predefine some constant values.  e.g. True == 1
    Defines { constantDefinitions :: [(String, Integer)] }
  
  -- | Turns on the imports extension.  Imported files are searched
  --   for in the directories given.
  | Imports { importDirs :: [AbsDir] }
                     deriving (Show, Eq)

-- | Trivial error type
newtype ParseError = ParseError String

instance Show ParseError where
    show (ParseError str) = str

-- | Parse a string.  The Imports language extension is not available
--   via this parser since it doesn't run in the IO Monad (fix this).
parseString :: [LanguageOptions] -> ByteString -> String -> Either [ParseError] Specification
parseString options txt source =
  case runParser specification (initContext defines) source txt of
    Left err -> Left [ParseError . show $ err]
    Right spec -> Right spec
  where
    defines = concat [cs | (Defines cs) <- options]

-- | Parse a file.  The Imports language extension is available.
parseFile :: [LanguageOptions] -> AbsFile -> IO (Either [ParseError] Specification)
parseFile options path = 
  parseImportSpecification defines includes path
  where
    defines = concat [cs | (Defines cs) <- options]
    includes = concat [cs | (Imports cs) <- options]
  
{-  do
  input <- B.readFile path'
  return $ parseString options input path'
    where
      path' = getPathString' path
-}

data ImportSpec = ImportSpec [RelFile] [Definition]

-- FIXME: using ExceptionT IO would simplify this
parseImportSpecification :: [(String, Integer)] -> [AbsDir] -> AbsFile -> IO (Either [ParseError] Specification)
parseImportSpecification defines includes path = do
  txt <- B.readFile path'
  result <-  runParserT (importSpec includes) (initContext defines) path' txt
  case result of
    Left errs -> return $ Left [ParseError . show $ errs]
    Right ispec -> return $ Right ispec
  where
    path' = getPathString' path

data Context = Context { constTable :: Map String ConstExpr
                       , nextEnum :: Integer
                       }

initContext :: [(String, Integer)] -> Context
initContext defines = Context (M.fromList . map (second ConstLit) $ defines) 0

-- type Parser = GenParser Char Context
type Parser = ParsecT ByteString Context

constPrim :: (Monad m) => Parser m ConstExpr
constPrim = (ConstLit <$> integer)
            <|> (ConstLit <$> boolean)
            <|> (findReference =<< identifier)

-- 3.4 Boolean
-- Interpret TRUE and FALSE as const-literals.

boolean :: (Monad m) => Parser m Integer
boolean = (1 <$ string "TRUE") <|> (0 <$ string "FALSE")

findReference :: (Monad m) => String -> Parser m ConstExpr
findReference n = do
  c <- getState
  case M.lookup n (constTable c) of
    Nothing -> unexpected $ "unknown constant '" ++ n ++ "'"
    Just e -> return . ConstRef . ConstantDef n $ e

-- Operator Precedence:
-- Unary: + - ~
-- Multiplicative: * / %
-- Additive: + -
-- Shift: << >>
-- Bitwise AND: &
-- Bitwise XOR: ^
-- Bitwise OR: |

constExpr :: (Monad m) => Parser m ConstExpr
constExpr = buildExpressionParser table term
    where
      table = [ [ prefix "+" id
                , prefix "-" (ConstUnExpr NEG)
                , prefix "~" (ConstUnExpr NOT)
                ]
              , [ binary "*" (ConstBinExpr MUL) AssocLeft
                , binary "/" (ConstBinExpr DIV) AssocLeft
                , binary "%" (ConstBinExpr MOD) AssocLeft
                ]
              , [ binary "+" (ConstBinExpr ADD) AssocLeft
                , binary "-" (ConstBinExpr SUB) AssocLeft
                ]
              , [ binary "<<" (ConstBinExpr SHL) AssocLeft
                , binary ">>" (ConstBinExpr SHR) AssocLeft
                ]
              , [ binary "&" (ConstBinExpr AND) AssocLeft
                ]
              , [ binary "^" (ConstBinExpr XOR) AssocLeft
                ]
              , [ binary "|" (ConstBinExpr OR) AssocLeft
                ]
              ]

      prefix name fun = Prefix (mkOp name fun)
      binary name fun = Infix (mkOp name fun)
      mkOp name fun = reservedOp name *> pure fun

      term = constPrim <|> parens constExpr

simpleType :: (Monad m) => String -> Type -> Parser m Type
simpleType keyword t = const t <$> try (reserved keyword)

typeSpec :: (Monad m) => Parser m Type
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

enumTypeSpec :: (Monad m) => Parser m Type
enumTypeSpec = TEnum <$> (reserved "enum" *> enumDetail)

enumDetail :: (Monad m) => Parser m EnumDetail
enumDetail = setNextEnum 0 *> braces body
    where
      body = EnumDetail <$> commaSep pair
      pair = do
        n <- identifier
        mc <- optionMaybe (reservedOp "=" *> constExpr)
        mkElem n mc

      setNextEnum n = do
        ctxt <- getState
        setState $ ctxt { nextEnum = n }

      incNextEnum = do
        ctxt <- getState
        let v = nextEnum ctxt
        setState $ ctxt { nextEnum = succ v }
        return v

      mkElem n Nothing = incNextEnum >>= insertConst n . ConstLit
      mkElem n (Just e) =
        let v = evalConstExpr e in (setNextEnum (v + 1)) *> insertConst n e

insertConst name e = do
    ctxt <- getState
    let table = constTable ctxt

    -- maybe the constant has already been defined ?  We only
    -- complain if they're trying to give it a different value.
    case M.lookup name table of
      Nothing -> insert name e table ctxt
      Just v' -> unexpected . concat $ [ "multiple definitions for "
                                       , name
                                       ]
    pure $ ConstantDef name e
  where
    insert name v table ctxt = setState $ ctxt { constTable = M.insert name v table }

structTypeSpec :: (Monad m) => Parser m Type
structTypeSpec = TStruct <$> (reserved "struct" *> structDetail)

structDetail :: (Monad m) => Parser m StructDetail
structDetail = braces body
    where body = StructDetail <$> many1 (declaration <* semi)

unionTypeSpec :: (Monad m) => Parser m Type
unionTypeSpec = TUnion <$> (reserved "union" *> unionDetail)

infixl 4 <*-*>
(<*-*>) :: (Applicative f) => f (a -> b -> c) -> f (a, b) -> f c
(<*-*>) fn fab = uncurry <$> fn <*> fab

unionDetail :: (Monad m) => Parser m UnionDetail
unionDetail = UnionDetail <$> switch <*-*> braces ((,) <$> caseStatements <*> deflt)
    where
      switch         = reserved "switch" *> parens declaration
      caseStatements = many1 caseStatement
      caseStatement  = (,) <$> (reserved "case" *> constExpr <* colon) <*> declaration <* semi
      deflt          = optionMaybe (reserved "default" *> colon *> declaration <* semi)

declaration :: (Monad m) => Parser m Decl
declaration =
    choice [ pure DeclVoid <* reserved "void"
           , try (mkString <$> (reserved "string" *> identifier) <*> angles (optionMaybe constExpr))
           , (reserved "opaque" *> identifier) >>= mkOpaque
           , typeSpec >>= mkBasicOrPointer
           ] <?> "declaration"
    where
      mkBasicOrPointer :: (Monad m) => Type -> Parser m Decl
      mkBasicOrPointer t = choice [ mkPointer t <$> (reservedOp "*" *> identifier)
                                  , identifier >>= mkBasic t
                                  ]

      mkBasic :: (Monad m) => Type -> String -> Parser m Decl
      mkBasic t n = choice [ mkArray t n <$> squares constExpr
                           , mkVarArray t n <$> angles (optionMaybe constExpr)
                           , pure (mkSimple t n)
                           ]

      mkOpaque :: (Monad m) => String -> Parser m Decl
      mkOpaque n = choice [ mkFixedOpaque n <$> squares constExpr
                          , mkVarOpaque n <$> angles (optionMaybe constExpr)
                          ]

      mkSimple t n    = Decl n $ DeclSimple t
      mkArray t n     = Decl n . DeclArray t
      mkVarArray t n  = Decl n . DeclVarArray t
      mkFixedOpaque n = Decl n . DeclOpaque
      mkVarOpaque n   = Decl n . DeclVarOpaque
      mkString n      = Decl n . DeclString
      mkPointer t n   = Decl n $ DeclPointer t

constantDef :: (Monad m) => Parser m ConstantDef
constantDef = ((,) <$> (reserved "const" *> identifier <* reservedOp "=") <*> constExpr) >>= uncurry insertConst

typeDef :: (Monad m) => Parser m Typedef
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

definition :: (Monad m) => Parser m Definition
definition = ((DefConstant <$> constantDef) <|> (DefTypedef <$> typeDef)) <* semi

withInput :: ByteString -> Parser IO a -> Parser IO a
withInput new p = do
  old <- getInput
  oldCtxt <- getState
  setInput new
  r <- p
  setInput old
  putState oldCtxt
  return r

module' :: (Monad m) => Parser m Module
module' = Module <$> sepBy1 segment (reservedOp ".")
    where
      segment = (:) <$> letter <*> many (alphaNum <|> char '_')

moduleStatement :: (Monad m) => Parser m Module
moduleStatement = reserved "module" *> module' <* semi

moduleToRelFile :: Module -> RelFile
moduleToRelFile (Module elements) =
    ((Data.List.foldl (</>) (asRelDir "") . map asRelDir $ prefix) </>
     (asRelFile file)) <.> "xdr"
    where
      prefix = reverse . tail . reverse $ elements
      file = head . reverse $ elements

importStatement :: [AbsDir] -> Parser IO ((Module, Specification), Context)
importStatement includes = do
  mod <- reserved "import" *> module' <* semi
  let path = moduleToRelFile mod
  mpath <- liftIO $ pathLookup includes path
  case mpath of
    Nothing -> unexpected $ "couldn't find import '" ++ getPathString' path ++ "'"
    Just path' -> do
      let pathTxt = getPathString' path'
      txt <- liftIO $ B.readFile pathTxt
      withInput txt ((\s c -> ((mod, s), c)) <$> importSpec includes <*> getState)

-- An xdr file that contains imports
importSpec :: [AbsDir] -> Parser IO Specification
importSpec includes = do
  ctxt <- getState
  mod <- whiteSpace *> moduleStatement
  specs <- whiteSpace *> many (importStatement includes)

  -- we need to combine all the contexts generated by the imports.
  ctxt' <- foldrM combineImports ctxt . map snd $ specs

  -- parse the rest of the file in this new uber context
  putState ctxt'
  Specification _ _ defs <- specification
  return $ Specification mod (M.fromList . map fst $ specs) defs
  
combineImports :: Context -> Context -> Parser IO Context
combineImports (Context m1 _) (Context m2 _) = return $ Context (M.union m1 m2) 0
  
-- FIXME: any defines passed in from the command line will appear as duplicates, so the following code needs updating
  -- if not . null $ duplicates
  -- then fail $ "duplicate constant definitions: " ++ show duplicates
  -- else return $ Context (M.union m1 m2) 0
  -- where
  --   duplicates = M.toList $ M.intersection m1 m2

specification :: (Monad m) => Parser m Specification
specification = Specification (Module ["global"]) M.empty <$> (whiteSpace *> many definition <* eof)

----------------------------------------------------------------