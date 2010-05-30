module Data.XDR.PPUtils
       ( ppConstExpr
       ) where


import Data.XDR.AST
import Text.PrettyPrint.Leijen as PP hiding (braces, indent)

ppConstExpr :: ConstExpr -> Doc
ppConstExpr (ConstLit n) = text . show $ n
ppConstExpr (ConstRef (ConstantDef n _)) = text n
ppConstExpr (ConstBinExpr op c1 c2) = ppBinOp op (ppConstExpr c1) (ppConstExpr c2)
ppConstExpr (ConstUnExpr NEG c) = parens $ text "-" <> ppConstExpr c
ppConstExpr (ConstUnExpr NOT c) = parens $ text "~" <> ppConstExpr c

ppBinOp :: BinOp -> Doc -> Doc -> Doc
ppBinOp op d1 d2 = parens $ parens d1 </> (text . symbol $ op) <+> parens d2
        where
          symbol MUL = "*"
          symbol DIV = "/"
          symbol MOD = "%"
          symbol ADD = "+"
          symbol SUB = "-"
          symbol SHR = ">>"
          symbol SHL = "<<"
          symbol AND = "&"
          symbol XOR = "^"
          symbol OR = "|"
