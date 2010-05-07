module Data.XDR.PrettyPrintC
    ( ppCHeader
    , ppCImpl
    ) where

import Data.Char
import qualified Data.ByteString.Lazy as B
import qualified Data.Digest.Pure.MD5 as MD5
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

md5 :: String -> Doc
md5 = text . show . MD5.md5 . B.pack . map (fromIntegral . ord)

block :: [String] -> Doc
block = foldr ((<$>) . text) empty

ppCHeader :: Maybe AbsFile -> Specification -> String
ppCHeader _ spec = show $ header <--> ppSpec spec <--> ppFuncs spec <--> footer
    where
      header = vcat [ text "#ifndef" <+> compileGuard
                    , text "#define" <+> compileGuard
                    , text ""
                    , text "#include <stdint.h>"
                    ]
      footer = text "#endif"
      compileGuard = text "XDR_" <> (md5 . show . typeCode $ spec) <> text "_H"

      ppSpec (Specification defs) = vcat . punctuate linebreak . map ppDef $ defs

      ppDef (DefConstant cd) = ppConstDef cd
      ppDef (DefTypedef td) = ppTypedef td

      ppConstDef (ConstantDef n c) = text "#define" <+> text n <+> ppConstExpr c

      ppTypedef (Typedef n ti) = text "typedef" <+> ppTypedefInternal n ti <> semi

      ppTypedefInternal n (DefSimple di) = ppDecl (Decl n di)
      ppTypedefInternal n (DefEnum ed) = text "enum" <+> ppEnumDetail ed <+> text n
      ppTypedefInternal n (DefStruct sd) = text "struct" <+> ppStructDetail sd <+> text n
      ppTypedefInternal n (DefUnion ud) = text "struct" <+> ppUnionDetail ud <+> text n

      ppEnumDetail (EnumDetail xs) = braces . punctuate comma . map ppEnumDef $ xs
      ppEnumDef (n, c) = text n <+> text "=" <+> ppConstPrim c

      ppStructDetail (StructDetail decls) = semiBraces . map ppDecl $ decls

      ppUnionDetail (UnionDetail selector cases mDefault) =
          semiBraces [ ppDecl selector
                     , text "union" <+>
                       (semiBraces $
                        ((map (ppDecl . snd) cases) ++ [ppOptional ppDecl mDefault])) <+> text "u"
                 ]

      ppDecl (Decl n (DeclSimple t)) = ppType t <+> text n
      ppDecl (Decl n (DeclArray t c)) = ppType t <+> text n <> (brackets . ppConstExpr $ c)
      ppDecl (Decl n (DeclVarArray t mc)) = ppVarStruct n t
      ppDecl (Decl n (DeclOpaque c)) = text "opaque" <+> text n <> (brackets . ppConstExpr $ c)
      ppDecl (Decl n (DeclVarOpaque mc)) = ppVarStruct n (TTypedef "void")
      ppDecl (Decl n (DeclString mc)) = text "char *" <> text n
      ppDecl (Decl n (DeclPointer t)) = ppType t <> text "*" <+> text n
      ppDecl DeclVoid = text "void"

      ppVarStruct n t = text "struct" <+> semiBraces [ text "uint32_t len"
                                                     , ppType t <+> text "*elts"
                                                     ] <+> text n

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

      ppFuncs (Specification defs) = thunkCode <$>
                                     (vcat . map declareFuncs . catMaybes . map getTypedef $ defs)

      getTypedef (DefTypedef (Typedef n _)) = Just n
      getTypedef _ = Nothing

      declareFuncs n = text ("int XDR_pack_" ++ n ++ "(struct packer *p, " ++ n ++
                             " *x, void **data, size_t *len, struct xdr_thunk *release);") <$>
                       text (n ++ " *XDR_unpack_" ++ n ++ "(struct unpacker *u, struct xdr_thunk *release);")

ppCImpl :: Maybe AbsFile -> Specification -> String
ppCImpl _ spec = show $ foldr (<-->) empty [cIncludes, poolCode, packerCode, unpackerCode, basicTypeMarshalling]

(<-->) :: Doc -> Doc -> Doc
b <--> e = vcat [ b
                , text ""
                , text "/*----------------------------------------------------------------*/"
                , text ""
                , e
                ]

----------------------------------------------------------------

-- Lots of literal C code follows
thunkCode :: Doc
thunkCode =
    block [ "#ifndef XDR_THUNK_DEFINED"
          , "#define XDR_THUNK_DEFINED"
          , ""
          , "struct xdr_thunk {"
          , "        void (*fn)(void *);"
          , "        void *context;"
          , "};"
          , ""
          , "static inline void XDR_run_thunk(struct xdr_thunk *t) {"
          , "        t->fn(t->context);"
          , "}"
          , ""
          , "#endif"
          ]

cIncludes :: Doc
cIncludes =
    block [ "#include <arpa/inet.h>"
          ]

poolCode =
    block [ "/* Pool memory allocator */"
          , "struct chunk {"
          , "        struct chunk *next;"
          , "        void *alloc_ptr;"
          , "        size_t free;"
          , "};"
          , ""
          , "struct pool {"
          , "        size_t chunk_size;"
          , "        struct chunk *chunks;"
          , "};"
          , ""
          , "/* alignment of memory returned by the allocator */"
          , "#define ALIGN __alignof__ (double)"
          , "#define ALIGN_MASK (ALIGN - 1)"
          , ""
          , "/* FIXME: make ptr maths portable (via a macro ?) */"
          , "static inline void *align_(void *ptr)"
          , "{"
          , "        uintptr_t offset = (uintptr_t) ptr & ALIGN_MASK;"
          , "        return offset ? ptr + (ALIGN - offset) : ptr;"
          , "}"
          , ""
          , "static inline void *inc_ptr_(void *ptr, size_t len)"
          , "{"
          , "        return align_(ptr + len);"
          , "}"
          , ""
          , "static struct chunk *new_chunk(size_t len)"
          , "{"
          , "        struct chunk *n = malloc(sizeof(*n) + len + ALIGN);"
          , ""
          , "        if (n) {"
          , "                n->alloc_ptr = align_((void *) (n + 1));"
          , "                n->free = len;"
          , "        }"
          , ""
          , "        return n;"
          , "}"
          , ""
          , "/* assumes there is enough space */"
          , "static inline void *alloc_from_chunk(struct chunk *c, size_t len)"
          , "{"
          , "        void *ptr = c->alloc_ptr;"
          , "        c->free -= len;"
          , "        c->alloc_ptr = inc_ptr_(c->alloc_ptr, len);"
          , "        return ptr;"
          , "}"
          , ""
          , "static struct pool *pool_create(size_t chunk_size)"
          , "{"
          , "        struct pool *mem = malloc(sizeof(*mem));"
          , "        if (mem) {"
          , "                struct chunk *c;"
          , ""
          , "                mem->chunk_size = chunk_size;"
          , ""
          , "                c = new_chunk(mem->chunk_size);"
          , "                if (!c) {"
          , "                        free(mem);"
          , "                        return NULL;"
          , "                }"
          , ""
          , "                c->next = NULL;"
          , "                mem->chunks = c;"
          , "        }"
          , ""
          , "        return mem;"
          , "}"
          , ""
          , "static void pool_destroy(struct pool *mem)"
          , "{"
          , "        struct chunk *c, *n;"
          , ""
          , "        for (c = mem->chunks; c; c = n) {"
          , "                n = c->next;"
          , "                free(c);"
          , "        }"
          , ""
          , "        free(mem);"
          , "}"
          , ""
          , "static inline void *pool_alloc_big(struct pool *mem, size_t len)"
          , "{"
          , "        struct chunk *c = new_chunk(len);"
          , "        if (c) {"
          , "                /*"
          , "                 * Put the new chunk after the head of the list, so we"
          , "                 * don't waste the space in the current head."
          , "                 */"
          , "                c->next = mem->chunks->next;"
          , "                mem->chunks->next = c;"
          , "                return alloc_from_chunk(c, len);"
          , "        }"
          , ""
          , "        return NULL;"
          , "}"
          , ""
          , "static inline void *pool_alloc_simple(struct pool *mem, size_t len)"
          , "{"
          , "        struct chunk *c = mem->chunks;"
          , "        if (c->free < len) {"
          , "                c = new_chunk(mem->chunk_size);"
          , "                if (!c)"
          , "                        return NULL;"
          , ""
          , "                c->next = mem->chunks;"
          , "                mem->chunks = c;"
          , "        }"
          , ""
          , "        return alloc_from_chunk(c, len);"
          , "}"
          , ""
          , "static inline void *pool_alloc(struct pool *mem, size_t len)"
          , "{"
          , "        if (len >= mem->chunk_size / 8)"
          , "                return pool_alloc_big(mem, len);"
          , "        else"
          , "                return pool_alloc_simple(mem, len);"
          , "}"
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

basicTypeMarshalling :: Doc
basicTypeMarshalling =
    block [ "static int pack_int(struct packer *p, int32_t x)"
          , "{"
          , "        return pack_uint(p, (uint32_t) x);"
          , "}"
          , ""
          , "static int unpack_int(struct unpacker *u, int32_t *x)"
          , "{"
          , "        return unpack_uint(u, (uint32_t *) x);"
          , "}"
          , ""
          , "static int pack_uint(struct packer *p, uint32_t x)"
          , "{"
          , "        uint32_t packed = htonl(x);"
          , "        packer_write(p, &packed, sizeof(packed));"
          , "        return 1;"
          , "}"
          , ""
          , "static int unpack_uint(struct unpacker *u, uint32_t *x)"
          , "{"
          , "        uint32_t data;"
          , ""
          , "        if (!unpacker_read(u, &data, sizeof(data)))"
          , "                return 0;"
          , ""
          , "        *x = ntohl(data);"
          , "        return 1;"
          , "}"
          , ""
          , "static int pack_hyper(struct packer *p, int64_t x)"
          , "{"
          , "        return pack_uhyper(p, (uint64_t) x);"
          , "}"
          , ""
          , "static int unpack_hyper(struct unpacker *u, int64_t *x)"
          , "{"
          , "        return unpack_uhyper(u, (uint64_t *) x);"
          , "}"
          , ""
          , "static int pack_uhyper(struct packer *p, uint64_t x)"
          , "{"
          , "        return pack_uint(p, x >> 32) && pack_uint(p, x & (uint32_t) -1);"
          , "}"
          , ""
          , "static int unpack_uhyper(struct unpacker *u, uint64_t *x)"
          , "{"
          , "        uint32_t hi, lo;"
          , ""
          , "        if (!unpack_uint(u, &hi) || !unpack_uint(u, &lo))"
          , "                return 0;"
          , ""
          , "        *x = hi;"
          , "        *x <<= 32;"
          , "        *x |= lo;"
          , "        return 1;"
          , "}"
          , ""
          , "/* FIXME: floats and doubles are packed in native format for now (lazy programmer). */"
          , "union float_cast {"
          , "        float f;"
          , "        uint32_t u;"
          , "};"
          , ""
          , "static int pack_float(struct packer *p, float x)"
          , "{"
          , "        union float_cast fc;"
          , "        fc.f = x;"
          , "        return pack_uint(p, fc.u);"
          , "}"
          , ""
          , "static int unpack_float(struct unpacker *u, float *x)"
          , "{"
          , "        union float_cast fc;"
          , "        if (!unpack_uint(u, &fc.u))"
          , "                return 0;"
          , ""
          , "        *x = fc.f;"
          , "        return 1;"
          , "}"
          , ""
          , "union double_cast {"
          , "        double f;"
          , "        uint64_t u;"
          , "};"
          , ""
          , "static int pack_double(struct packer *p, double x)"
          , "{"
          , "        union double_cast fc;"
          , "        fc.f = x;"
          , "        return pack_uhyper(p, fc.u);"
          , "}"
          , ""
          , "static int unpack_double(struct unpacker *u, double *x)"
          , "{"
          , "        union float_cast fc;"
          , "        if (!unpack_uhyper(u, &fc.u))"
          , "                return 0;"
          , ""
          , "        *x = fc.f;"
          , "        return 1;"
          , "}"
          ]
