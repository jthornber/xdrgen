module Data.XDR.PrettyPrintJava
    ( ppJava
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
import Text.PrettyPrint.Leijen as PP hiding (semiBraces, braces, indent)

-- | Bird.

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

-- | Bird.

cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
cross (f, g) = pair (f . fst, g . snd)

indent :: Int
indent = 4

lrparen :: Doc
lrparen = lparen <> rparen

braces :: [Doc] -> Doc
braces ds = nest indent (lbrace <$> vcat ds) <$> rbrace

semiBraces :: [Doc] -> Doc
semiBraces = braces . map (<> semi)

block :: [String] -> Doc
block = foldr ((<$>) . (<> semi) . text) empty

maybePush :: Maybe a -> [a] -> [a]
maybePush =
    maybe id (:)

camelCase :: String -> String
camelCase [] = []
camelCase ('_':x:xs) = toUpper x : camelCase xs
camelCase (x:xs) = x : camelCase xs

typeName :: String -> String
typeName [] = []
typeName (x:xs) = toUpper x : camelCase xs

topName :: Module -> String
topName (Module elements) =
    typeName $ last elements

upperName :: String -> String
upperName = map toUpper

-- | DeclPair is Decl without DeclVoid.

data DeclPair = DeclPair
    { declName :: String
    , declInternal :: DeclInternal
    }

declToPair :: Decl -> Maybe DeclPair
declToPair (Decl n di) = Just $ DeclPair n di
declToPair DeclVoid = Nothing

declsToPairs :: [Decl] -> [DeclPair] -> [DeclPair]
declsToPairs =
    flip (foldr f)
  where
    f (Decl n di) = (DeclPair n di :)
    f _ = id

defsToPairs :: [Definition] -> [DeclPair] -> [DeclPair]
defsToPairs =
    flip (foldr f)
  where
    f (DefTypedef (Typedef k v)) =
        (DeclPair k (typedefToDecl v) :)
    f _ = id

type DeclMap = Map String DeclPair

specToDeclMap :: Specification -> DeclMap -> DeclMap
specToDeclMap (Specification _ imports defs) m =
    defsToDeclMap defs m'
  where
    m' = foldr specToDeclMap m (M.elems imports)

defsToDeclMap :: [Definition] -> DeclMap -> DeclMap
defsToDeclMap =
    flip (foldr f)
  where
    f (DefTypedef (Typedef k v)) =
        M.insert k $ DeclPair k (typedefToDecl v)
    f _ = id

typedefToDecl :: TypedefInternal -> DeclInternal
typedefToDecl (DefSimple di) = di
typedefToDecl (DefEnum ed) = DeclSimple (TEnum ed)
typedefToDecl (DefStruct sd) = DeclSimple (TStruct sd)
typedefToDecl (DefUnion ud) = DeclSimple (TUnion ud)

-- JType

data JType = JType
    { jType :: Doc
    , jUnbox :: Doc
    , jConstPrim :: ConstPrim -> Doc
    }

lookupPair :: DeclMap -> DeclPair -> JType
lookupPair m (DeclPair n (DeclSimple t)) =
    lookupType m n t

lookupPair m (DeclPair n (DeclArray t _)) =
    JType td td ppConstPrim
  where
    td = text "Array" <> langle <> jType jt <> rangle
    jt = lookupType m n t

lookupPair m (DeclPair n (DeclVarArray t mc)) =
    JType td td ppConstPrim
  where
    td = text "Array" <> langle <> jType jt <> rangle
    jt = lookupType m n t

lookupPair m (DeclPair n (DeclOpaque c)) =
    JType td td ppConstPrim
  where
    td = text "Opaque"

lookupPair m (DeclPair n (DeclVarOpaque mc)) =
    JType td td ppConstPrim
  where
    td = text "Opaque"

lookupPair m (DeclPair n (DeclString mc)) =
    JType td td ppConstPrim
  where
    td = text "String"

lookupPair m (DeclPair n (DeclPointer t)) =
    lookupType m n t

lookupType :: DeclMap -> String -> Type -> JType

lookupType _ _ TInt = JType (text "Integer") kInt ppConstPrim
lookupType _ _ TUInt = JType (text "Integer") kInt ppConstPrim
lookupType _ _ THyper = JType (text "Long") kLong ppConstPrim
lookupType _ _ TUHyper = JType (text "Long") kLong ppConstPrim
lookupType _ _ TFloat = JType (text "Float") kFloat ppConstPrim
lookupType _ _ TDouble = JType (text "Double") kDouble ppConstPrim
lookupType _ _ TQuadruple = error "not supported"
lookupType _ _ TBool =
    JType (text "Boolean") kBoolean f
  where
    f = (text "0 !=" <+>) . ppConstPrim
lookupType _ _ (TEnum _) = JType (text "Integer") kInt ppConstPrim
lookupType _ n (TStruct _) =
    JType td td ppConstPrim
  where
    td = text $ typeName n

lookupType m n (TUnion (UnionDetail sel _ _)) =
    JType td td ppConstPrim
  where
    td = text "Union" <> langle <> maybe (text "Integer") jType jt <> rangle
    jt = lookupPair m `fmap` declToPair sel

lookupType m _ (TTypedef n) =
    JType td td cp
  where
    td = maybe (text $ typeName n) jType jt
    cp = maybe ppConstPrim jConstPrim jt
    jt = lookupPair m `fmap` M.lookup n m

ppCodecPair :: DeclMap -> DeclPair -> Doc
ppCodecPair m (DeclPair n (DeclSimple t)) =
    ppCodecType m n t
ppCodecPair m (DeclPair n (DeclArray t c)) =
    text "XdrArray.newCodec"
             <> tupled [ppCodecType m n t, ppConstExpr c]
ppCodecPair m (DeclPair n (DeclVarArray t mc)) =
    text "XdrArray.newVarCodec"
             <> tupled [ppCodecType m n t,
                        maybe (text "Integer.MAX_VALUE") ppConstExpr mc]
ppCodecPair m (DeclPair n (DeclOpaque c)) =
    text "XdrOpaque.newCodec" <> tupled [ppConstExpr c]
ppCodecPair m (DeclPair n (DeclVarOpaque mc)) =
    text "XdrOpaque." <> maybe (text "VAR_CODEC") ppVarCodec mc
ppCodecPair m (DeclPair n (DeclString mc)) =
    text "XdrString." <> maybe (text "VAR_CODEC") ppVarCodec mc
ppCodecPair m (DeclPair n (DeclPointer t)) =
    text "XdrOptional.newCodec" <> tupled [ppCodecType m n t]

ppCodecType :: DeclMap -> String -> Type -> Doc
ppCodecType _ _ TInt = text "XdrInt.CODEC"
ppCodecType _ _ TUInt = text "XdrInt.CODEC"
ppCodecType _ _ THyper = text "XdrHyper.CODEC"
ppCodecType _ _ TUHyper = text "XdrHyper.CODEC"
ppCodecType _ _ TFloat = text "XdrFloat.CODEC"
ppCodecType _ _ TDouble = text "XdrDouble.CODEC"
ppCodecType _ _ TQuadruple = error "not supported"
ppCodecType _ _ TBool = text "XdrBool.CODEC"
ppCodecType _ _ (TEnum _) = text "XdrInt.CODEC"
ppCodecType _ n (TStruct _) = text ("Xdr" ++ typeName n ++ ".CODEC")
ppCodecType m n (TUnion (UnionDetail sel _ mDef)) =
    text "XdrUnion.newCodec"
             <> tupled ((fromMaybe (text "XdrInt.CODEC") sd)
                        : text "CASES" : maybeToList dd)
  where
    sd = ppCodecPair m `fmap` declToPair sel
    dd = ppCodecPair m `fmap` dp
    dp = declToPair =<< mDef

ppCodecType _ _ (TTypedef n) =
    text ("Xdr" ++ typeName n ++ ".CODEC")

ppVarCodec :: ConstExpr -> Doc
ppVarCodec c =
    text "newVarCodec" <> tupled [ppConstExpr c]

-- Java

ppMaybePackage :: Module -> Maybe Doc
ppMaybePackage (Module elements) =
    if null xs then Nothing
    else Just $ kPackage <+> f xs <> semi
  where
    f = text . concat . intersperse "."
    xs = init elements

ppImports :: Doc
ppImports = block ["import java.nio.ByteBuffer"]

ppClass :: String -> [Doc] -> Doc
ppClass n =
    (lead <+>) . braces . (ppPrivateCons n :)
  where
    lead = kClass <+> text tn
    tn = typeName n

ppPrivateCons :: String -> Doc
ppPrivateCons n =
    kPrivate <+> text tn <> lrparen <+> lbrace <$> rbrace
  where
    tn = typeName n

ppConstDef n d =
    kPublic <+> kStatic <+> kFinal <+> kInt <+> text n' <+> equals
                <+> d <> semi
  where
    n' = upperName n

ppSimpleCodec :: DeclMap -> DeclPair -> Doc
ppSimpleCodec m p@(DeclPair n d) =
    kPublic <+> kStatic <+> kFinal
                <+> ppClass ("xdr_" ++ n) [ppPublicCodec td cd]
  where
    td = jType $ lookupPair m p
    cd = ppCodecPair m p

ppPublicCodec :: Doc -> Doc -> Doc
ppPublicCodec t =
    (kPublic <+>) . ppStaticCodec "CODEC" t

ppPrivateCodec :: String -> Doc -> Doc -> Doc
ppPrivateCodec n t =
    (kPrivate <+>) . ppStaticCodec un t
  where
    un = upperName n ++ "_CODEC"

ppStaticCodec :: String -> Doc -> Doc -> Doc
ppStaticCodec n t d =
    nest indent (decl <+> equals </> d) <> semi
  where
    decl = kStatic <+> kFinal <+> text "Codec"
           <> langle <> t <> rangle <+> text n

ppIface :: String -> [Doc] -> Doc
ppIface n =
    (lead <+>) . braces
  where
    lead = kInterface <+> text tn
    tn = typeName n

ppEnumBody :: [(String, ConstPrim)] -> [Doc]
ppEnumBody =
    map f
  where
    f (n, c) = ppConstDef n (text . show $ evalConstPrim c)

ppIfaceBody :: [(String, JType)] -> [Doc]
ppIfaceBody =
    map ppGetterDecl

ppGetterName :: String -> Doc
ppGetterName n =
    text ("get" ++ typeName n)

ppGetterDecl :: (String, JType) -> Doc
ppGetterDecl (n, jt) =
    jType jt <+> ppGetterName n <> lrparen <> semi

ppAnonCodec :: String -> [Doc] -> Doc
ppAnonCodec n =
    (nest indent lead <+>) . (<> semi) . braces
  where
    lead = kPublic <+> kStatic <+> kFinal <+> ct <+> text "CODEC" <+> equals
           </> kNew <+> ct <> lrparen
    ct = text "Codec" <> langle <> text tn <> rangle
    tn = typeName n

ppCodecDecls :: DeclMap -> [DeclPair] -> [Doc]
ppCodecDecls m =
    map (ppCodecDecl m)

ppCodecDecl :: DeclMap -> DeclPair -> Doc
ppCodecDecl m p@(DeclPair n d) =
    ppPrivateCodec n td (ppCodecPair m p)
  where
    td = jType $ lookupPair m p

ppEncode :: String -> [Doc] -> Doc
ppEncode n =
    (nest indent lead <+>) . semiBraces
  where
    lead = kPublic <+> kFinal <+> kVoid <+> text "encode"
           <> tupled [text "ByteBuffer buf", text (tn ++ " val")]
           </> kThrows <+> text "CharacterCodingException"
    tn = typeName n

ppDecode :: String -> [Doc] -> Doc
ppDecode n =
    (nest indent lead <+>) . semiBraces
  where
    lead = kPublic <+> kFinal <+> text tn <+> text "decode"
           <> tupled [text "ByteBuffer buf"]
           </> kThrows <+> text "CharacterCodingException"
    tn = typeName n

ppEncodeVars :: [(String, JType)] -> [Doc]
ppEncodeVars =
    map ppEncodeVar

ppEncodeVar :: (String, JType) -> Doc
ppEncodeVar (n, jt) =
    text un <> text "_CODEC.encode"
             <> tupled [text "buf", text ("val.get" ++ tn) <> lrparen]
  where
    tn = typeName n
    un = upperName n

ppDecodeVars :: [(String, JType)] -> [Doc]
ppDecodeVars =
    map ppDecodeVar

ppDecodeVar :: (String, JType) -> Doc
ppDecodeVar (n, jt) =
    kFinal <+> jType jt <+> text (cn ++ "_") <+> equals
               <+> text un <> text "_CODEC.decode" <> tupled [text "buf"]
  where
    cn = camelCase n
    un = upperName n

ppInner :: String -> [Doc] -> Doc
ppInner n =
    (lead <+>) . braces
  where
    lead = kReturn <+> kNew <+> text tn <> lrparen
    tn = typeName n

ppInnerBody :: [(String, JType)] -> [Doc]
ppInnerBody =
    map ppGetterImpl

ppGetterImpl :: (String, JType) -> Doc
ppGetterImpl (n, jt) =
    kPublic <+> kFinal <+> jType jt
                <+> ppGetterName n <> lrparen
                <+> nest indent (lbrace <$> body) <$> rbrace
  where
    body = kReturn <+> text (cn ++ "_") <> semi
    cn = camelCase n

ppJava :: Specification -> String
ppJava spec =
    show . vcat $ maybePush (ppMaybePackage m) [ppImports, body]
  where
    body = kPublic <+> kFinal <+> ppClass tn (ppSpec spec)
    tn = topName m
    m = moduleName spec

    ppSpec (Specification _ _ defs) =
        f defs
      where
        f = punctuate linebreak . map (ppDef m)
        m = specToDeclMap spec $ M.empty

    ppDef m (DefConstant (ConstantDef n c)) =
        ppConstDef n $ ppConstExpr c
    ppDef m (DefTypedef (Typedef n ti)) =
        ppDecl m n $ typedefToDecl ti

    ppDecl m n d@(DeclSimple (TEnum ed)) =
        ppEnumDetail n ed
            <$> (ppSimpleCodec m (DeclPair n d))
    ppDecl m n d@(DeclSimple (TStruct sd)) =
        ppStructDetail m n sd
    ppDecl m n d@(DeclSimple (TUnion ud)) =
        kPublic <+> kStatic <+> kFinal
                    <+> ppClass ("xdr_" ++ n)
                            [ppUnionCases m ud, ppPublicCodec td cd]
      where
        td = jType $ lookupPair m dp
        cd = ppCodecPair m dp
        dp = DeclPair n d

    ppDecl m n d =
        ppSimpleCodec m $ DeclPair n d

    -- | Enumerated values must be visible in the top-level namespace, so they
    -- are not enclosed in a separate class or interface.

    ppEnumDetail n (EnumDetail pairs) =
        vcat $ ppEnumBody pairs

    ppStructDetail m n (StructDetail decls) =
        ppIface n (ppIfaceBody jts)
                    <$> kPublic <+> kStatic <+> kFinal
                    <+> ppClass ("xdr_" ++ n) (ppAnonCodec n ([enc, dec])
                                               : ppCodecDecls m dps)
      where
        enc = ppEncode n (ppEncodeVars jts)
        dec = ppDecode n (ppDecodeVars jts
                          ++ [ppInner n (ppInnerBody jts)])
        jts = map (pair (declName, lookupPair m)) dps
        dps = declsToPairs decls []

    ppUnionCases m (UnionDetail sel cases mDef) =
        nest indent lead <> semi
      where
        lead = kPrivate <+> kStatic <+> kFinal <+> text "Map" <> langle
               <> (maybe (text "Integer") jType jt) <> char ',' <+> text "Codec" <> langle
               <> char '?' <> rangle <> rangle <+> text "CASES" <+> equals
               </> text "XdrUnion.newCases"
               <> tupled (ppUnionPairs m (maybe ppConstPrim jConstPrim jt) cases)
        jt = lookupPair m `fmap` sp
        sp = declToPair sel

    ppUnionPairs m f =
        foldr (g . ppUnionPair m f) []
      where
        g (c, d) = (c :) . (d :)

    ppUnionPair m f (c, d) =
        (f c, fromMaybe (text "XdrVoid.CODEC") cd)
      where
        cd = (ppCodecPair m) `fmap` declToPair d
