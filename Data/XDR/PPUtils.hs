module Data.XDR.PPUtils
       ( ppConstExpr
       , ppConstPrim
       ) where


import Data.XDR.AST
import Text.PrettyPrint.Leijen as PP hiding (braces, indent)

ppConstExpr :: ConstExpr -> Doc
ppConstExpr (CEPrim p) = ppConstPrim p
ppConstExpr (CEBinExpr op c1 c2) = ppBinOp op (ppConstExpr c1) (ppConstExpr c2)
ppConstExpr (CEUnExpr NEGATE c) = parens $ text "-" <> ppConstExpr c

ppBinOp :: BinOp -> Doc -> Doc -> Doc
ppBinOp op d1 d2 = parens $ parens d1 <+> (text . symbol $ op) <+> parens d2
        where
          symbol PLUS = "+"
          symbol MINUS = "-"
          symbol MULT = "*"
          symbol DIV = "/"

ppConstPrim :: ConstPrim -> Doc
ppConstPrim (ConstLit n) = text . show $ n
ppConstPrim (ConstDefRef (ConstantDef n _)) = text n
ppConstPrim (ConstEnumRef n _) = text n
