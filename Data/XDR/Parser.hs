module Data.XDR.Parser
    ( parseString
    , parseFile
    , ParseError
    ) where

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Text.ParserCombinators.Parsec as Parsec hiding (ParseError, Parser)
import Text.ParserCombinators.Parsec.Expr

import Data.XDR.AST

----------------------------------------------------------------

-- FIXME: add location detail
data ParseError = ParseError String

instance Show ParseError where
    show (ParseError str) = str

parseString :: [(String, Integer)] -> String -> String -> Either [ParseError] Specification
parseString defines txt source =
    case runParser xdrParser (initContext defines) source txt of
      Left err -> Left [ParseError . show $ err]
      Right spec -> Right spec

parseFile :: [(String, Integer)] -> FilePath -> IO (Either [ParseError] Specification)
parseFile defines path = do
  input <- readFile path
  return $ parseString defines input path

data Context = Context { constTable :: Map String Integer
                       , nextEnum :: Integer
                       }

initContext :: [(String, Integer)] -> Context
initContext defines = Context (M.fromList defines) 0

type Parser = GenParser Char Context

----------------------------------------------------------------

semi :: Parser ()
semi = text ";"

discard :: Parser a -> Parser ()
discard = (>> return ())

comment :: Parser ()
comment = try multiComment <|> singleComment

singleComment :: Parser ()
singleComment = discard (try (string "//") *> skipMany (satisfy (/= '\n')))

multiComment :: Parser ()
multiComment = try (string "/*") >> inMultiComment

inMultiComment :: Parser ()
inMultiComment = (discard $ try (string "*/")) <|>
                 (multiComment >> inMultiComment) <|>
                 (skipMany1 (noneOf startEnd) >> inMultiComment) <|>
                 (oneOf startEnd >> inMultiComment)
    where
      startEnd = "/*"

whitespace :: Parser ()
whitespace = (spaces *> skipMany (comment *> spaces)) <?> "whitespace"

-- This consumes the whitespace _after_ a token, if we'd consumed
-- before we'd have to use 'try' everywhere since consuming
-- whitespace would cause a partial match and subsequent backtrack.
tok :: Parser a -> Parser a
tok p = p <* whitespace

text :: String -> Parser ()
text txt = discard (tok $ string txt)

keywords :: Set String
keywords = S.fromList [ "bool", "case", "const", "default", "double", "quadruple", "enum"
                      , "float", "hyper", "opaque", "string", "struct", "switch", "typedef"
                      , "union", "unsigned", "void", "int"
                      ]

identifier :: Parser String
identifier = (try ident >>= lookup) <?> "identifier"
    where
      ident = tok $ (:) <$> letter <*> many (char '_' <|> alphaNum)
      lookup t = if S.member t keywords then unexpected "keyword" else pure t

number :: Parser Integer
number = tok (readNumber <$> optionMaybe (char '-') <*> many1 digit)
    where
      readNumber Nothing xs = read xs
      readNumber (Just _) xs = - (read xs)

intConstant :: Parser Integer
intConstant = do
  ((identifier >>= check) <|> number) <?> "constant"
    where
      check name = do
                ctxt <- getState
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

      prefix name fun = Prefix (do { text name; return fun })
      binary name fun assoc = Infix (do { text name; return fun }) assoc

      term = parens constExpr <|> intConstant 
      parens body = text "(" *> body <* text ")"

constant :: Parser Constant
constant = ConstLit <$> constExpr

bra :: String -> String -> Parser a -> Parser a
bra b e p = text b *> p <* text e

simpleType :: String -> Type -> Parser Type
simpleType keyword t = const t <$> try (text keyword)

typeSpec :: Parser Type
typeSpec = choice [ try (text "unsigned" *> (simpleType "int" TUInt <|> simpleType "hyper" TUHyper))
                  , simpleType "int" TInt
                  , simpleType "hyper" THyper
                  , simpleType "float" TFloat
                  , simpleType "double" TDouble
                  , simpleType "quad" TQuad
                  , simpleType "bool" TBool
                  , try enumTypeSpec
                  , try structTypeSpec
                  , try unionTypeSpec
                  , TTypedef <$> identifier
                  ] <?> "type"

enumTypeSpec :: Parser Type
enumTypeSpec = TEnum <$> (text "enum" *> enumDetail)

enumDetail :: Parser EnumDetail
enumDetail = setNextEnum 0 *> bra "{" "}" body
    where
      body = EnumDetail <$> sepBy pair (text ",")
      pair = ((,) <$> identifier <*> optionMaybe (text "=" *> constant)) >>= uncurry mkElem

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
structTypeSpec = TStruct <$> (text "struct" *> structDetail)

structDetail :: Parser StructDetail
structDetail = bra "{" "}" body
    where body = StructDetail <$> many1 (declaration <* semi)

unionTypeSpec :: Parser Type
unionTypeSpec = TUnion <$> (text "union" *> unionDetail)

unionDetail :: Parser UnionDetail
unionDetail = UnionDetail <$> switch <*> (text "{" *> caseStatements) <*> (deflt <* text "}")
    where
      switch         = text "switch" *> bra "(" ")" declaration
      caseStatements = many1 caseStatement
      caseStatement  = (,) <$> bra "case" ":" constant <*> declaration <* semi
      deflt          = optionMaybe (text "default" *> text ":" *> declaration <* semi)

declaration :: Parser Decl
declaration =
    choice [ pure DeclVoid <* text "void"
           , try (mkString <$> (text "string" *> identifier) <*> bra "<" ">" (optionMaybe constant))
           , (text "opaque" *> identifier) >>= mkOpaque
           , typeSpec >>= mkBasicOrPointer
           ] <?> "declaration"
    where
      mkBasicOrPointer :: Type -> Parser Decl
      mkBasicOrPointer t = choice [ mkPointer t <$> (text "*" *> identifier)
                                  , identifier >>= mkBasic t
                                  ]

      mkBasic :: Type -> String -> Parser Decl
      mkBasic t n = choice [ mkArray t n <$> bra "[" "]" constant
                           , mkVarArray t n <$> bra "<" ">" (optionMaybe constant)
                           , pure (mkSimple t n)
                           ]

      mkOpaque :: String -> Parser Decl
      mkOpaque n = choice [ mkFixedOpaque n <$> bra "[" "]" constant
                          , mkVarOpaque n <$> bra "<" ">" (optionMaybe constant)
                          ]

      mkSimple t n    = Decl n $ DeclSimple t
      mkArray t n     = Decl n . DeclArray t
      mkVarArray t n  = Decl n . DeclVarArray t
      mkFixedOpaque n = Decl n . DeclOpaque
      mkVarOpaque n   = Decl n . DeclVarOpaque
      mkString n      = Decl n . DeclString
      mkPointer t n   = Decl n $ DeclPointer t

constantDef :: Parser ConstantDef
constantDef = do
  n <- bra "const" "=" identifier
  c <- constant
  mkConst n c
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
typeDef = choice [ mkSimple <$> (text "typedef" *> declaration)
                 , mkEnum   <$> (text "enum" *> identifier) <*> enumDetail
                 , mkStruct <$> (text "struct" *> identifier) <*> structDetail
                 , mkUnion  <$> (text "union" *> identifier) <*> unionDetail
                 ] <?> "typedef"
    where
      mkSimple (Decl n di) = Typedef n $ DefSimple di
      mkSimple _           = error "internal error"
      mkEnum n             = Typedef n . DefEnum
      mkStruct n           = Typedef n . DefStruct
      mkUnion n            = Typedef n . DefUnion

definition :: Parser Definition
definition = ((DefConstant <$> constantDef) <|> (DefTypedef <$> typeDef)) <* semi

xdrParser :: Parser Specification
xdrParser = Specification <$> (whitespace *> many definition <* eof)

----------------------------------------------------------------