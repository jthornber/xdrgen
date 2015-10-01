-- | Not complete yet.  Will be replaced by some templates.
module Data.XDR.PrettyPrintCPP
    ( ppCPPHeader
    , ppCPPImpl
    ) where

import Data.Char
import Data.List
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Data.TypeHash
import Data.XDR.AST
import Data.XDR.PPUtils
import System.Path
import Text.PrettyPrint.Leijen as PP hiding (semiBraces, braces, indent)

----------------------------------------------------------------

indent :: Int
indent = 8

ppOptional :: (a -> Doc) -> Maybe a -> Doc
ppOptional _ Nothing = empty
ppOptional fn (Just a) = fn a

braces :: [Doc] -> Doc
braces ds = nest indent (lbrace <$> vcat ds) <$> rbrace

semiBraces :: [Doc] -> Doc
semiBraces = braces . map (<> semi)

block :: [String] -> Doc
block = foldr ((<$>) . text) empty

joinSections :: String -> Doc -> Doc -> Doc
joinSections txt b e = vcat [ b
                            , text ""
                            , text txt
                            , text ""
                            , e ]

(<--->) = joinSections "//----------------------------------------------------------------"
(<-->) = joinSections "//--------------------------------"
b <-> e = b <$> text "" <$> e

textFragments :: [String] -> Doc
textFragments = foldr1 (<>) . map text

mname spec separator = case moduleName spec of
  Module xs -> concat . intersperse separator $ xs

encodeFn :: String -> Doc
encodeFn n = textFragments [ "void encode(xdr_write_buffer::ptr const &buf, "
                           , n
                           , " const &x)"
                           ]

decodeFn :: String -> Doc
decodeFn n = textFragments [ "void decode(xdr_read_buffer::ptr const &buf, "
                           , n
                           , " &x)"
                           ]

----------------------------------------------------------------

ppCPPHeader :: Specification -> String
ppCPPHeader spec = show $ header <---> body <---> footer
    where
      body = namespace (mname spec "_") (ppUsings <-> ppSpec spec <--> ppFuncs spec)
      namespace nm d = text "namespace" <+> text nm <+> braces [d]

      header = vcat [ text "#ifndef" <+> compileGuard
                    , text "#define" <+> compileGuard
                    , text ""
                    , text "#include \"xdr/xdr.h\""
                    ]
      footer = text "#endif"
      compileGuard = text "XDR_" <> text mnameUpper <> text "_H"

      mnameUpper = map toUpper $ mname spec "_"

      ppUsings = text "using namespace xdr;"

      ppSpec (Specification _ _ defs) = vcat . punctuate linebreak . map ppDef $ defs

      ppDef (DefConstant cd) = ppConstantDef cd
      ppDef (DefTypedef td) = ppTypedef td
      ppConstantDef (ConstantDef n c) = text "#define" <+> text n <+> ppConstExpr c

      ppTypedef (Typedef n ti) = text "typedef" <+> ppTypedefInternal n ti <> semi

      ppTypedefInternal n (DefSimple di) = ppDecl' (Decl n di) False
      ppTypedefInternal n (DefEnum ed) = text "enum" <+> ppEnumDetail ed <+> text n
      ppTypedefInternal n (DefStruct sd) = text "struct" <+> ppStructDetail sd <+> text n
      ppTypedefInternal n (DefUnion ud) = text "struct" <+> ppUnionDetail ud <+> text n

      ppEnumDetail (EnumDetail xs) = braces . punctuate comma . map ppEnumDef $ xs
      ppEnumDef (ConstantDef n c) = text n <+> text "=" <+> ppConstExpr c

      ppStructDetail (StructDetail decls) = semiBraces . map ppDecl $ decls

      ppUnionDetail (UnionDetail selector cases mDefault) =
          semiBraces [ ppDecl selector
                     , text "union" <+>
                       semiBraces
                        (map (ppDecl . snd) cases ++ [ppOptional ppDecl mDefault]) <+> text "u"
                 ]

      ppDecl' (Decl n (DeclSimple t)) underscore = ppType t <+> varName n underscore
      ppDecl' (Decl n (DeclArray t c)) underscore = ppType t <+> varName n underscore <> (brackets . ppConstExpr $ c)
      ppDecl' (Decl n (DeclVarArray t mc)) underscore = ppVarStruct n t
      ppDecl' (Decl n (DeclOpaque c)) underscore = text "xdr_opaque" <+> varName n underscore <> (brackets . ppConstExpr $ c)
      ppDecl' (Decl n (DeclVarOpaque mc)) underscore = ppVarStruct n (TTypedef "xdr_opaque")
      ppDecl' (Decl n (DeclString mc)) underscore = text "char *" <> varName n underscore
      ppDecl' (Decl n (DeclPointer t)) underscore = ppType t <> text "*" <+> varName n underscore
      ppDecl' DeclVoid _ = text "void"

      ppDecl d = ppDecl' d True

      ppVarStruct n t = text "std::vector<" <> ppType t <> text ">" <+> varName n True

      varName n True = text n <> text "_"
      varName n False = text n

      ppType TInt = text "int32_t"
      ppType TUInt = text "uint32_t"
      ppType THyper = text "int64_t"
      ppType TUHyper = text "uint64_t"
      ppType TFloat = text "float"
      ppType TDouble = text "double"
      ppType TQuadruple = text "long double"
      ppType TBool = text "bool"
      ppType (TEnum ed) = text "enum" <+> ppEnumDetail ed
      ppType (TStruct sd) = text "struct" <+> ppStructDetail sd
      ppType (TUnion ud) = text "struct" <+> ppUnionDetail ud
      ppType (TTypedef n) = text n

      ppFuncs (Specification _ _ defs) = (vcat . intersperse (text "") . map declareFuncs . mapMaybe getTypedef $ defs)

      getTypedef (DefTypedef (Typedef n _)) = Just n
      getTypedef _ = Nothing

      declareFuncs n = (encodeFn n <> text ";") <$>
                       (decodeFn n <> text ";")

----------------------------------------------------------------

ppCPPImpl :: Specification -> String
ppCPPImpl spec = show $ foldr (<--->) empty
                 [ includeHeader spec
                 , cIncludes
                 , packerCode spec
                 , unpackerCode
                 ]

----------------------------------------------------------------

includeHeader :: Specification -> Doc
includeHeader spec = text (show spec) <$> text "#include \"" <> (text $ mname spec "/") <> text ".h\""

cIncludes :: Doc
cIncludes =
    block [ "#include <arpa/inet.h>"
          ]

packerCode :: Specification -> Doc
packerCode (Specification _ _ defs) = (vcat . intersperse (text "") . map encodeDef . mapMaybe getTypedef $ defs)
  where
    getTypedef (DefTypedef t) = Just t
    getTypedef _ = Nothing

    encodeDef (Typedef n tdi) = encodeFn n <$> braces [encodeBody tdi]

    encodeBody (DefSimple di) = encodeBody' di
    encodeBody (DefEnum _) = fixme "DefEnum"
    encodeBody (DefStruct (StructDetail fields)) = vcat . map encodeStructField $ fields
    encodeBody (DefUnion _) = fixme "DefUnion"

    encodeBody' (DeclSimple t) = text "buf.encode(" <> ppType t <> text ");"
    encodeBody' (DeclArray t expr) = fixme "array"
    encodeBody' (DeclVarArray t mexpr) = fixme "var array"
    encodeBody' _ = finish

    encodeStructField (Decl n (DeclSimple t)) = text "encode(buf, " <> text n <> text "_);"
    encodeStructField (Decl n (DeclArray t expr)) = text "encode(buf, " <> text n <> text "_);"
    encodeStructField (Decl n (DeclVarArray t mexpr)) = braces
      [ text "uint32_t len;" 
      , text "buf.decode(len);"
      , text "for (unsigned i = 0; i < len; i++)"
      , braces [ text n
               , text 
        
               ]
      , text "encode_array(buf, " <> text n <> text "_);"
    encodeStructField (Decl n (DeclOpaque expr)) = text "encode_var_array(buf, " <> text n <> text "_);"
    encodeStructField (Decl n (DeclVarOpaque expr)) = text "encode_var_opaque(buf, " <> text n <> text "_);"
    encodeStructField (Decl n (DeclString mexpr)) = text "encode_string(buf, " <> text n <> text "_);"
    encodeStructField (Decl n (DeclPointer t)) = text "encode_ptr(buf, " <> text n <> text "_);"
    encodeStructField DeclVoid = text ""

    ppType :: Type -> Doc
    ppType TInt = text "int32_t"
    ppType TUInt = text "uint32_t"
    ppType THyper = text "int64_t"
    ppType TUHyper = text "uint64_t"
    ppType TFloat = text "float"
    ppType TDouble = text "double"
    ppType TQuadruple = text "long double"
    ppType TBool = text "bool"
    ppType (TEnum ed) = finish
    ppType (TStruct sd) = fixme "struct"
    ppType (TUnion ud) = finish
    ppType (TTypedef n) = text n

unpackerCode :: Doc
unpackerCode = fixme "write unpacking code"

fixme :: String -> Doc
fixme txt = text "// FIXME:" <+> text txt

finish = fixme "finish"
