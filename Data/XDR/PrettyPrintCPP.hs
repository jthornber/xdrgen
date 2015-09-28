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

----------------------------------------------------------------

ppCPPHeader :: Specification -> String
ppCPPHeader spec = show $ header <---> body <---> footer
    where
      body = namespace mname (ppUsings <-> ppSpec spec <--> ppFuncs spec)
      namespace nm d = text "namespace" <+> text nm <+> braces [d]

      header = vcat [ text "#ifndef" <+> compileGuard
                    , text "#define" <+> compileGuard
                    , text ""
                    , text "#include \"xdr/xdr.h\""
                    ]
      footer = text "#endif"
      compileGuard = text "XDR_" <> text mnameUpper <> text "_H"

      mname = case moduleName spec of
        Module xs -> concat . intersperse "_" $ xs
        
      mnameUpper = map toUpper $ mname

      ppUsings = text "using namespace xdr;"

      ppSpec (Specification _ _ defs) = vcat . punctuate linebreak . map ppDef $ defs

      ppDef (DefConstant cd) = ppConstantDef cd
      ppDef (DefTypedef td) = ppTypedef td

      ppConstantDef (ConstantDef n c) = text "#define" <+> text n <+> ppConstExpr c

      ppTypedef (Typedef n ti) = text "typedef" <+> ppTypedefInternal n ti <> semi

      ppTypedefInternal n (DefSimple di) = ppDecl (Decl n di)
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

      ppDecl (Decl n (DeclSimple t)) = ppType t <+> varName n
      ppDecl (Decl n (DeclArray t c)) = ppType t <+> varName n <> (brackets . ppConstExpr $ c)
      ppDecl (Decl n (DeclVarArray t mc)) = ppVarStruct n t
      ppDecl (Decl n (DeclOpaque c)) = text "xdr_opaque" <+> varName n <> (brackets . ppConstExpr $ c)
      ppDecl (Decl n (DeclVarOpaque mc)) = ppVarStruct n (TTypedef "xdr_opaque")
      ppDecl (Decl n (DeclString mc)) = text "char *" <> varName n
      ppDecl (Decl n (DeclPointer t)) = ppType t <> text "*" <+> varName n
      ppDecl DeclVoid = text "void"

      ppVarStruct n t = text "std::vector<" <> ppType t <> text ">" <+> varName n

      varName n = text n <> text "_"

      ppType TInt = text "int32_t"
      ppType TUInt = text "uint32_t"
      ppType THyper = text "int64_t"
      ppType TUHyper = text "uint64_t"
      ppType TFloat = text "float"
      ppType TDouble = text "double"
      ppType TQuadruple = text "long double"
      ppType TBool = text "int32_t"
      ppType (TEnum ed) = text "enum" <+> ppEnumDetail ed
      ppType (TStruct sd) = text "struct" <+> ppStructDetail sd
      ppType (TUnion ud) = text "struct" <+> ppUnionDetail ud
      ppType (TTypedef n) = text n

      ppFuncs (Specification _ _ defs) = (vcat . intersperse (text "") . map declareFuncs . mapMaybe getTypedef $ defs)

      getTypedef (DefTypedef (Typedef n _)) = Just n
      getTypedef _ = Nothing

      declareFuncs n = text_fragments [ "void encode_"
                                      , n
                                      , "(xdr_write_buffer::ptr const &buf, "
                                      , n
                                      , " const &x);"
                                      ] <$>
                       text_fragments [ "void decode_"
                                      , n
                                      , "(xdr_read_buffer::ptr const &buf, "
                                      , n
                                      , " &x);"
                                      ]
                       
      text_fragments = foldr1 (<>) . map text


----------------------------------------------------------------

ppCPPImpl :: Specification -> String
ppCPPImpl spec = show $ foldr (<--->) empty [cIncludes, packerCode, unpackerCode]


----------------------------------------------------------------

cIncludes :: Doc
cIncludes =
    block [ "#include <arpa/inet.h>"
          , "#include <vector>"
          ]

packerCode :: Doc
packerCode =
    block [ "struct packer {"
          , "        size_t allocated;"
          , "        size_t written;"
          , "        void *data;"
          , "        void *current;"
          , "};"
          , ""
          , "static struct packer *packer_create(size_t packed_size_hint)"
          , "{"
          , "        struct packer *p = malloc(sizeof(*p));"
          , ""
          , "        if (!p)"
          , "                return NULL;"
          , ""
          , "        p->allocated = packed_size_hint < 1024 ? 1024 : packed_size_hint;"
          , "        p->written = 0;"
          , "        p->data = malloc(p->allocated);"
          , "        if (!p->data) {"
          , "                free(p);"
          , "                return NULL;"
          , "        }"
          , "        p->current = p->data;"
          , ""
          , "        return p;"
          , "}"
          , ""
          , "static void packer_destroy(struct packer *p)"
          , "{"
          , "        free(p->data);"
          , "        free(p);"
          , "}"
          , ""
          , "static void packer_peek(struct packer *p, void **data, size_t *len)"
          , "{"
          , "        *data = p->data;"
          , "        *len = p->written;"
          , "}"
          , ""
          , "static inline int packer_ensure(struct packer *p, size_t len)"
          , "{"
          , "        if (p->allocated - p->written < len) {"
          , "                void *new_data;"
          , ""
          , "                new_data = realloc(p->data, p->allocated * 2);"
          , "                if (!new_data)"
          , "                        return 0;"
          , ""
          , "                p->allocated *= 2;"
          , "                p->data = new_data;"
          , "                p->current = p->data + p->written;"
          , "        }"
          , ""
          , "        return 1;"
          , "}"
          , ""
          , "static inline int packer_write(struct packer *p, void *data, size_t len)"
          , "{"
          , "        if (!packer_ensure(p, len))"
          , "                return 0;"
          , ""
          , "        memcpy(p->current, data, len);"
          , "        p->current += len;"
          , "        p->written += len;"
          , "        return 1;"
          , "}"
          ]

unpackerCode = block [ "struct unpacker {"
                     , "        struct pool *mem;"
                     , "        void *data;"
                     , "        size_t len;"
                     , "};"
                     , ""
                     , "static struct unpacker *unpacker_create(void *data, size_t len)"
                     , "{"
                     , "        struct unpacker *u = malloc(sizeof(*u));"
                     , "        if (!u)"
                     , "                return NULL;"
                     , ""
                     , "        u->mem = pool_create(len * 2);"
                     , "        if (!u->mem) {"
                     , "                free(u);"
                     , "                return NULL;"
                     , "        }"
                     , ""
                     , "        u->data = data;"
                     , "        u->len = len;"
                     , ""
                     , "        return u;"
                     , "}"
                     , ""
                     , "static void unpacker_destroy(struct unpacker *u)"
                     , "{"
                     , "        pool_destroy(u->mem);"
                     , "        free(u);"
                     , "}"
                     , ""
                     , "static inline int unpacker_read(struct unpacker *u, void *data, size_t len)"
                     , "{"
                     , "        if (len > u->len)"
                     , "                return 0;"
                     , ""
                     , "        memcpy(data, u->data, len);"
                     , "        u->len -= len;"
                     , "        u->data += len;"
                     , "        return 1;"
                     , "}"
                     ]
