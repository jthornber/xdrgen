-- | FIXME: Mark describe sun rpc/openxdr.

module Data.XDR.PrettyPrintRpc
    ( ppRpcHeader
    , ppRpcImpl
    ) where

import Control.Monad
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.XDR.AST
import Data.XDR.PPKeywords
import Data.XDR.PPUtils
import System.Path hiding ((</>))
import Text.PrettyPrint.Leijen as PP hiding (semiBraces, braces, indent)

indent :: Int
indent = 4

braces :: [Doc] -> Doc
braces ds =
    nest indent (lbrace <$> vcat ds) <$> rbrace

semiBraces :: [Doc] -> Doc
semiBraces =
    braces . map (<> semi)

switchBraces :: [Doc] -> Doc
switchBraces =
    f . map (<> semi)
  where
    f ds = lbrace <$> vcat ds <$> rbrace

moduleToString :: Module -> String -> String
moduleToString (Module elements) sep =
    concat . intersperse sep $ elements

fileGuard :: Module -> Doc
fileGuard mod = text ("XDR_" ++ map toUpper (moduleToString mod "_") ++ "_H")

getTypedefs :: [Definition] -> [Typedef]
getTypedefs =
    foldr f []
  where
    f (DefTypedef td) tds = td : tds
    f _ tds = tds

liftMaybeToList :: (a -> b) -> Maybe a -> [b]
liftMaybeToList f =
    maybeToList . liftM f

maybeEmpty :: Maybe Doc -> Doc
maybeEmpty =
    fromMaybe empty

maybeSemiBraces :: [Maybe Doc] -> Doc
maybeSemiBraces =
    semiBraces . catMaybes

-- XDR
ppDefaultConstant :: Doc -> Maybe ConstExpr -> Doc
ppDefaultConstant d =
    maybe d ppConstExpr

ppSizeOf :: Type -> Doc
ppSizeOf t =
    kSizeof <> parens (ppType t)

ppIfDecode :: Doc
ppIfDecode =
    kIf <+> parens (text "XDR_DECODE == xdrs->x_op")

ppIfFalse :: Doc -> Doc
ppIfFalse d =
    kIf <+> parens (char '!' <> d </> text "&& XDR_FREE != xdrs->x_op")

ppReturn :: String -> Doc
ppReturn =
    (kReturn <+>) . text

ppGoto :: (Show a) => a -> Doc
ppGoto n =
    kGoto <+> text ("xfree" ++ show n)

ppIfFalseReturn :: Doc -> Doc
ppIfFalseReturn d =
    nest indent (ppIfFalse d <$> ppReturn "FALSE")

ppIfFalseGoto :: (Show a) => a -> Doc -> Doc
ppIfFalseGoto n d =
    nest indent (ppIfFalse d <$> ppGoto n)

ppUnwind :: (Show a) => a -> Doc -> Doc
ppUnwind n d =
    text ("xfree" ++ show n) <> colon
             <$> nest indent (ppIfDecode <$> d)

ppFuncParam :: String -> TypedefInternal -> Doc
ppFuncParam n (DefSimple (DeclArray t c)) =
    tupled [text "XDR *xdrs", text (n ++ " objp")]
ppFuncParam n (DefSimple (DeclOpaque c)) =
    tupled [text "XDR *xdrs", text (n ++ " objp")]
ppFuncParam n _ =
    tupled [text "XDR *xdrs", text (n ++ " *objp")]

ppFuncSig :: String -> TypedefInternal -> Doc
ppFuncSig n ti =
    text "bool_t" <$> text ("xdr_" ++ n) <> ppFuncParam n ti

ppCallEnum :: String -> String -> Doc
ppCallEnum xdrs ptr =
    text "xdr_enum"
             <> tupled [text xdrs,
                        text ("(enum_t *)" ++ ptr)]

ppCallType :: String -> String -> Type -> Doc
ppCallType xdrs ptr TBool =
    text "xdr_bool"
             <> tupled [text xdrs,
                        text ptr]
ppCallType xdrs ptr THyper =
    text "xdr_hyper"
             <> tupled [text xdrs,
                        text ptr]
ppCallType xdrs ptr TUHyper =
    text "xdr_u_hyper"
             <> tupled [text xdrs,
                        text ptr]

ppCallType xdrs ptr (TEnum _) =
    ppCallEnum xdrs ptr

ppCallType xdrs ptr (TStruct _) =
    undefined

ppCallType xdrs ptr (TUnion _) =
    undefined

ppCallType xdrs ptr t =
    text "xdr_" <> ppType t
             <> tupled [text xdrs,
                        text ptr]

ppCallVector :: String -> String -> ConstExpr -> Type -> Doc
ppCallVector xdrs ptr c t =
    text "xdr_vector"
             <> tupled [text xdrs,
                        text ("(char *)" ++ ptr),
                        ppConstExpr c,
                        ppSizeOf t,
                        text "(xdrproc_t)xdr_" <> ppType t]

ppCallArray :: String -> String -> String -> Maybe ConstExpr -> Type -> Doc
ppCallArray xdrs ptr len mc t =
    text "xdr_array"
             <> tupled [text xdrs,
                        text ("(char **)" ++ ptr),
                        text ("(u_int *)" ++ len),
                        ppDefaultConstant (text "~0") mc,
                        ppSizeOf t,
                        text "(xdrproc_t)xdr_" <> ppType t]

ppCallOpaque :: String -> String -> ConstExpr -> Doc
ppCallOpaque xdrs ptr c =
    text "xdr_opaque"
             <> tupled [text xdrs,
                        text ptr,
                        ppConstExpr c]

ppCallBytes :: String -> String -> String -> Maybe ConstExpr -> Doc
ppCallBytes xdrs ptr len mc =
    text "xdr_bytes"
             <> tupled [text xdrs,
                        text ("(char **)" ++ ptr),
                        text ("(u_int *)" ++ len),
                        ppDefaultConstant (text "~0") mc]

ppCallString :: String -> String -> Maybe ConstExpr -> Doc
ppCallString xdrs ptr mc =
    text "xdr_string"
             <> tupled [text xdrs,
                        text ptr,
                        ppDefaultConstant (text "~0") mc]

ppCallPointer :: String -> String -> Type -> Doc
ppCallPointer xdrs ptr t =
    text "xdr_pointer"
             <> tupled [text xdrs,
                        text ("(char **)" ++ ptr),
                        ppSizeOf t,
                        text "(xdrproc_t)xdr_" <> ppType t]

ppEnumBody :: [ConstantDef] -> Doc
ppEnumBody =
    braces . punctuate comma . map f
  where
    f (ConstantDef n c) = text n <+> text "=" <+> ppConstExpr c

ppStructBody :: [Decl] -> Doc
ppStructBody =
    maybeSemiBraces . map ppMaybeDecl

ppMaybeDecl :: Decl -> Maybe Doc
ppMaybeDecl (Decl n (DeclSimple t)) =
    Just $ ppType t <+> text n
ppMaybeDecl (Decl n (DeclArray t c)) =
    Just $ ppType t <+> text n <> (brackets . ppConstExpr $ c)
ppMaybeDecl (Decl n (DeclVarArray t mc)) =
    Just $ ppVarStruct n t
ppMaybeDecl (Decl n (DeclOpaque c)) =
    Just $ kChar <+> text n <> (brackets . ppConstExpr $ c)
ppMaybeDecl (Decl n (DeclVarOpaque mc)) =
    Just $ ppVarStruct n (TTypedef "char")
ppMaybeDecl (Decl n (DeclString mc)) =
    Just $ kChar <+> char '*' <> text n
ppMaybeDecl (Decl n (DeclPointer t)) =
    Just $ ppType t <+> char '*' <+> text n
ppMaybeDecl DeclVoid =
    Nothing

ppVarStruct :: String -> Type -> Doc
ppVarStruct n t =
    kStruct <+> semiBraces [text "u_int" <+> text "len",
                            ppType t <+> text "*val"]
                <+> text n

ppType :: Type -> Doc
ppType TInt = kInt
ppType TUInt = text "u_int"
ppType THyper = text "quad_t"
ppType TUHyper = text "u_quad_t"
ppType TFloat = kFloat
ppType TDouble = kDouble
ppType TQuadruple = error "not supported"
ppType TBool = text "bool_t"
ppType (TEnum (EnumDetail ed)) = text "enum" <+> ppEnumBody ed
ppType (TStruct sd) = error "unexpected struct"
ppType (TUnion ud) = error "unexpected union"
ppType (TTypedef n) = text n

ppIncludes :: [Module] -> Doc
ppIncludes =
    vcat . map ppInclude

ppInclude :: Module -> Doc
ppInclude mod =
    text "#include" <+> text f
  where
    f = "\"" ++ moduleToString mod "/" ++ ".h\""

-- | Pretty print a C header for use with the sun rpc library.
ppRpcHeader :: Specification -> String
ppRpcHeader spec =
    show $ header <$> ppSpec spec <$> ppFuncs spec <$> footer
  where
    header = vcat [text "#ifndef" <+> compileGuard,
                   text "#define" <+> compileGuard,
                   text "#include <rpc/xdr.h>",
                   ppIncludes . M.keys $ imports spec]
    footer = text "#endif /*" <+> compileGuard <+> text "*/"
    compileGuard = fileGuard $ moduleName spec

    ppSpec (Specification _ _ defs) =
        f defs
      where
        f = vcat . punctuate linebreak . map ppDef

    ppDef (DefConstant cd) = ppConstantDef cd
    ppDef (DefTypedef td) = ppTypedef td <> semi

    ppConstantDef (ConstantDef n c) =
        text "#define" <+> text n <+> ppConstExpr c

    ppTypedef (Typedef n ti) =
        kTypedef <+> ppTypedefInternal n ti

    ppTypedefInternal n (DefSimple (DeclSimple (TStruct sd))) =
        ppStructDetail n sd
    ppTypedefInternal n (DefSimple (DeclSimple (TUnion ud))) =
        ppUnionDetail n ud
    ppTypedefInternal n (DefSimple di) =
        maybeEmpty $ ppMaybeDecl (Decl n di)
    ppTypedefInternal n (DefEnum (EnumDetail ed)) =
        kEnum <+> text n <+> ppEnumBody ed <+> text n
    ppTypedefInternal n (DefStruct sd) =
        ppStructDetail n sd
    ppTypedefInternal n (DefUnion ud) =
        ppUnionDetail n ud

    ppStructDetail n (StructDetail decls) =
        sn <+> text n <> semi <$> sn <+> ppStructBody decls
      where
        sn = kStruct <+> text n

    ppUnionDetail n (UnionDetail selector cases mDefault) =
        sn <+> text n <> semi <$> sn <+> body
      where
        sn = kStruct <+> text n
        body = maybeSemiBraces [ppMaybeDecl selector,
                                Just $ kUnion <+> ubody <+> text "u"]
        ubody = maybeSemiBraces $ foldr ((:) . ppMaybeDecl . snd) [def] cases
        def = ppMaybeDecl =<< mDefault

    ppFuncs (Specification _ _ defs) =
        vcat . map f $ getTypedefs defs
      where
        f (Typedef n ti) = ppFuncSig n ti <> semi

-- | Pretty print a C implementation for use with the sun rpc library.
ppRpcImpl :: Specification -> String
ppRpcImpl spec = show $ ppInclude (moduleName spec) <$> ppSpec spec
  where
    ppSpec (Specification _ _ defs) =
        f defs
      where
        f = vcat . punctuate linebreak . map ppTypedef . getTypedefs

    ppTypedef (Typedef n ti) =
        ppFuncSig n ti <$> semiBraces (ppFuncBody n ti)

    ppFuncBody _ (DefSimple (DeclSimple (TStruct sd))) =
        ppStructDetail sd
    ppFuncBody _ (DefSimple (DeclSimple (TUnion ud))) =
        ppUnionDetail ud
    ppFuncBody n (DefSimple di) =
        f (Decl n di) ++ [ppReturn "TRUE"]
      where
        f = liftMaybeToList ppIfFalseReturn . ppSimpleCall "xdrs"
    ppFuncBody n (DefEnum ed) =
        [ppIfFalseReturn $ ppCallEnum "xdrs" "objp",
         ppReturn "TRUE"]
    ppFuncBody _ (DefStruct sd) = ppStructDetail sd
    ppFuncBody _ (DefUnion ud) = ppUnionDetail ud

    ppSimpleCall xdrs (Decl _ (DeclSimple t)) =
        Just $ ppCallType xdrs "objp" t
    ppSimpleCall xdrs (Decl _ (DeclArray t c)) =
        Just $ ppCallVector xdrs "objp" c t
    ppSimpleCall xdrs (Decl _ (DeclVarArray t mc)) =
        Just $ ppCallArray xdrs "&objp->val" "&objp->len" mc t
    ppSimpleCall xdrs (Decl _ (DeclOpaque c)) =
        Just $ ppCallOpaque xdrs "objp" c
    ppSimpleCall xdrs (Decl _ (DeclVarOpaque mc)) =
        Just $ ppCallBytes xdrs "&objp->val" "&objp->len" mc
    ppSimpleCall xdrs (Decl _ (DeclString mc)) =
        Just $ ppCallString xdrs "objp" mc
    ppSimpleCall xdrs (Decl _ (DeclPointer t)) =
        Just $ ppCallPointer xdrs "objp" t
    ppSimpleCall _ DeclVoid =
        Nothing

    ppStructDetail (StructDetail [decl@(Decl _ _)]) =
        f decl ++ [ppReturn "TRUE"]
      where
        f = liftMaybeToList ppIfFalseReturn . ppStructCall "xdrs"

    ppStructDetail (StructDetail decls) =
        text "XDR xfree" :
        text "xfree.x_op = XDR_FREE" :
        zipWith ppIfFalseGoto [0..] allocs
        ++ [ppReturn "TRUE"]
        ++ map (uncurry ppUnwind) (drop 1 . reverse $ zip [1..] frees)
        ++ [text "xfree0:" <$> ppReturn "FALSE"]
      where
        allocs = mapMaybe (ppStructCall "xdrs") decls
        frees = mapMaybe (ppStructCall "&xfree") decls

    ppStructCall xdrs (Decl n (DeclSimple t)) =
        Just $ ppCallType xdrs ("&objp->" ++ n) t
    ppStructCall xdrs (Decl n (DeclArray t c)) =
        Just $ ppCallVector xdrs ("&objp->" ++ n) c t
    ppStructCall xdrs (Decl n (DeclVarArray t mc)) =
        Just $ ppCallArray xdrs ("&objp->" ++ n ++ ".val")
                 ("&objp->" ++ n ++ ".len") mc t
    ppStructCall xdrs (Decl n (DeclOpaque c)) =
        Just $ ppCallOpaque xdrs ("objp->" ++ n) c
    ppStructCall xdrs (Decl n (DeclVarOpaque mc)) =
        Just $ ppCallBytes xdrs ("&objp->" ++ n ++ ".val")
                 ("&objp->" ++ n ++ ".len") mc
    ppStructCall xdrs (Decl n (DeclString mc)) =
        Just $ ppCallString xdrs ("&objp->" ++ n) mc
    ppStructCall xdrs (Decl n (DeclPointer t)) =
        Just $ ppCallPointer xdrs ("&objp->" ++ n) t
    ppStructCall _ DeclVoid =
        Nothing

    ppUnionDetail (UnionDetail selector@(Decl n _) cases mDefault) =
        [text "XDR xfree",
         text "xfree.x_op = XDR_FREE",
         ppIfFalseGoto 0 $ maybeEmpty $ ppStructCall "xdrs" selector,
         ppSwitch n cases mDefault <$> ppReturn "TRUE",
         ppUnwind 1 $ maybeEmpty $ ppStructCall "&xfree" selector,
         text "xfree0:" <$> ppReturn "FALSE"]

    ppSwitch n cases mDefault =
        kSwitch <+> (parens . text) ("objp->" ++ n) <+> switchBraces ds
      where
        ds = foldr ((:) . nest indent . ppSwitchCase) def cases
        def = liftMaybeToList (nest indent . ppSwitchDefault) mDefault

    ppSwitchCase (c, d) =
        enclose l r $ ppSwitchCall d
      where
        l = kCase <+> ppConstExpr c <> colon
        r = line <> kBreak

    ppSwitchDefault =
        enclose l r . ppSwitchCall
      where
        l = kDefault <> char ':'
        r = line <> kBreak

    ppSwitchCall =
        maybe empty f . ppUnionCall "xdrs"
      where
        f = enclose line semi . ppIfFalseGoto 1

    ppUnionCall xdrs (Decl n (DeclSimple t)) =
        Just $ ppCallType xdrs ("&objp->u." ++ n) t
    ppUnionCall xdrs (Decl n (DeclArray t c)) =
        Just $ ppCallVector xdrs ("&objp->u." ++ n) c t
    ppUnionCall xdrs (Decl n (DeclVarArray t mc)) =
        Just $ ppCallArray xdrs ("&objp->u." ++ n ++ ".val")
                 ("&objp->u." ++ n ++ ".len") mc t
    ppUnionCall xdrs (Decl n (DeclOpaque c)) =
        Just $ ppCallOpaque xdrs ("objp->u." ++ n) c
    ppUnionCall xdrs (Decl n (DeclVarOpaque mc)) =
        Just $ ppCallBytes xdrs ("&objp->u." ++ n ++ ".val")
                 ("&objp->u." ++ n ++ ".len") mc
    ppUnionCall xdrs (Decl n (DeclString mc)) =
        Just $ ppCallString xdrs ("&objp->u." ++ n) mc
    ppUnionCall xdrs (Decl n (DeclPointer t)) =
        Just $ ppCallPointer xdrs ("&objp->u." ++ n) t
    ppUnionCall _ DeclVoid =
        Nothing
