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

indentLine :: Doc -> Doc -> Doc
indentLine d1 d2 = nest indent (d1 <$> d2)

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

encodeFn :: Doc -> Doc -> Doc
encodeFn typeName paramName =
        foldr1 (<>) [ text "void encode(xdr_write_buffer::ptr const &buf, "
                    , typeName
                    , text " const &"
                    , paramName
                    , text ")"
                    ]

decodeFn :: Doc -> Doc -> Doc
decodeFn typeName paramName =
        text "void decode(xdr_read_buffer::ptr const &buf, "
        <> typeName
        <> text " &"
        <> paramName
        <> text ")"

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
      ppDecl' (Decl n (DeclOpaque c)) underscore = text "xdr_opaque" <+> varName n underscore
      ppDecl' (Decl n (DeclVarOpaque mc)) underscore = text "xdr_opaque" <+> varName n underscore
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

      declareFuncs n = (encodeFn typeName paramName <> text ";") <$>
                       (decodeFn typeName paramName <> text ";")
        where typeName = text n
              paramName = text "x"
----------------------------------------------------------------

ppCPPImpl :: Specification -> String
ppCPPImpl spec = show $ foldr (<--->) empty
                 [ includeHeader spec
                 , packerCode spec
                 , unpackerCode
                 ]

----------------------------------------------------------------

includeHeader :: Specification -> Doc
includeHeader spec = text "#include \"" <> (text $ mname spec "/") <> text ".h\""

packerCode :: Specification -> Doc
packerCode (Specification _ _ defs) = (vcat . intersperse (text "") . map encodeTypedef . mapMaybe getTypedef $ defs)
  where
    getTypedef (DefTypedef t) = Just t
    getTypedef _ = Nothing

    encodeTypedef (Typedef n tdi) = encodeFn (text n) paramName <$> braces [encodeTypedefInternal paramName tdi]
      where paramName = text "x"

    encodeTypedefInternal paramName (DefSimple di) = encodeDeclInternal paramName di
    encodeTypedefInternal paramName (DefEnum _) = fixme "DefEnum"
    encodeTypedefInternal paramName (DefStruct sd) = encodeStruct paramName sd
    encodeTypedefInternal paramName (DefUnion ud) = encodeUnion paramName ud

    encodeDecl (Decl n di) = encodeDeclInternal (varName n) di
    encodeDecl DeclVoid    = empty

    encodeDeclInternal v (DeclSimple t) = encodeType v t
    encodeDeclInternal v (DeclArray t expr) = forLoop i count (encodeType (subscript v i) t)
      where i = genSym "i"
            count = text . show . evalConstExpr $ expr
            subscript v n = v <> text "[" <> n <> text "]"

    encodeDeclInternal v (DeclVarArray t mexpr) = braces
      [ text "uint32_t " <> len <> text " = " <> v <> text ".size();"
      , guardArraySize mexpr
      , text "encode(buf, " <> len <> text ");"
      , forLoop i len $ text "encode(buf, " <> v <> text "[i]);"
      ]
      where i = genSym "i"
            len = genSym "len"

    encodeDeclInternal v (DeclOpaque expr) =
      text "encode_fixed(buf, " <> exprToDoc expr <> text ", " <> v <> text ");"

    encodeDeclInternal v (DeclVarOpaque Nothing) = text "encode_variable(buf, " <> v <> text ");"
    encodeDeclInternal v (DeclVarOpaque (Just expr)) = text "encode_variable(buf, " <> max <> text ", " <> v <> text ");"
        where max = exprToDoc expr

    encodeDeclInternal v (DeclString Nothing) = text "encode_string(buf, " <> v <> text ");"
    encodeDeclInternal v (DeclString (Just expr)) = text "encode_string(buf, " <> max <> text ", " <> v <> text ");"
        where max = exprToDoc expr

    encodeDeclInternal v (DeclPointer t) = text "encode_ptr(buf, " <> v <> text ");"

    guardArraySize :: Maybe ConstExpr -> Doc
    guardArraySize Nothing = empty
    guardArraySize (Just expr) = text "if (" <> len <> text " > " <>
     exprToDoc expr <>
     text ") " `indentLine` throw "array too large"

    exprToDoc = text . show . evalConstExpr

    encodeType :: Doc -> Type -> Doc
    encodeType v TInt = encodeImplicitType v
    encodeType v TUInt = encodeImplicitType v
    encodeType v THyper = encodeImplicitType v
    encodeType v TUHyper = encodeImplicitType v
    encodeType v TFloat = encodeImplicitType v
    encodeType v TDouble = encodeImplicitType v
    encodeType v TQuadruple = encodeImplicitType v
    encodeType v TBool = encodeImplicitType v
    encodeType v (TEnum ed) = encodeEnumDetail v ed
    encodeType v (TStruct sd) = encodeStruct v sd
    encodeType v (TUnion ud) = encodeUnion v ud
    encodeType v (TTypedef n) = encodeImplicitType v

    encodeImplicitType v = text "encode(buf, " <> v <> text ");"

    encodeEnumDetail v (EnumDetail consts) = text "encode(buf, static_cast<uint32_t>(" <> v <> text "));"

    encodeStruct v (StructDetail fields) = vcat . map encodeDecl $ fields
    encodeUnion v (UnionDetail (Decl discriminator _) branches mDefault) =
        text "encode(buf, " <> v <> text "." <> text discriminator <> text ");" <$>
        text "switch (" <> v <> text "." <> text discriminator <> text ")" <+>
          braces (map (encodeUnionBranch v) branches)

    encodeUnionBranch v (expr, decl) =
        nest indent (text "case " <> exprToDoc expr <> text ":" <$>
                     encodeDecl decl <$> text "break;" <$> text "")
    varName n = text n <> text "_"

    -- FIXME; remove
    len = genSym "len"

forLoop :: Doc -> Doc -> Doc -> Doc
forLoop var count body =
  text "for " <> var <> text " = 0; " <> var <> text " < " <> count <> text "; " <> var <> text "++)" `indentLine` body

forLoopStar :: Doc -> Doc -> [Doc] -> Doc
forLoopStar var count docs = forLoop var count (braces docs)

throw :: String -> Doc
throw txt = text "throw std::runtime_error(\"" <> text txt <> text "\");"

genSym :: String -> Doc
genSym str = text "___gensym_" <> text str

unpackerCode :: Doc
unpackerCode = fixme "write unpacking code"

fixme :: String -> Doc
fixme txt = text "// FIXME:" <+> text txt

finish = fixme "finish"
