{-# LANGUAGE DeriveDataTypeable #-}

module Data.XDR.AST
    ( ConstPrim (..)
    , BinOp (..)
    , UnOp (..)
    , ConstExpr (..)
    , DeclInternal (..)
    , Decl (..)
    , Type (..)
    , EnumDetail (..)
    , StructDetail (..)
    , UnionDetail (..)
    , TypedefInternal (..)
    , Typedef (..)
    , ConstantDef (..)
    , Definition (..)
    , Specification (..)
    , evalConstExpr
    , evalConstPrim
    ) where

import Data.Generics
import Data.Maybe

----------------------------------------------------------------

data ConstPrim = ConstLit Integer
               | ConstDefRef ConstantDef
               | ConstEnumRef String ConstPrim
               deriving (Show, Typeable, Data)

data BinOp = PLUS | MINUS | DIV | MULT
           deriving (Eq, Show, Typeable, Data)
                    
data UnOp = NEGATE
          deriving (Eq, Show, Typeable, Data)

data ConstExpr = CEPrim ConstPrim
               | CEBinExpr BinOp ConstExpr ConstExpr
               | CEUnExpr UnOp ConstExpr
               deriving (Show, Typeable, Data)

data DeclInternal = DeclSimple Type
                  | DeclArray Type ConstExpr
                  | DeclVarArray Type (Maybe ConstExpr)
                  | DeclOpaque ConstExpr
                  | DeclVarOpaque (Maybe ConstExpr)
                  | DeclString (Maybe ConstExpr)
                  | DeclPointer Type
                    deriving (Show, Typeable, Data)

data Decl = Decl String DeclInternal
          | DeclVoid deriving (Show, Typeable, Data)

data Type = TInt
          | TUInt
          | THyper
          | TUHyper
          | TFloat
          | TDouble
          | TQuadruple
          | TBool
          | TEnum EnumDetail
          | TStruct StructDetail
          | TUnion UnionDetail
          | TTypedef String
            deriving (Show, Typeable, Data)

newtype EnumDetail = EnumDetail [(String, ConstPrim)]
    deriving (Show, Typeable, Data)

newtype StructDetail = StructDetail [Decl]
    deriving (Show, Typeable, Data)

data UnionDetail = UnionDetail Decl [(ConstPrim, Decl)] (Maybe Decl) -- selector, cases, default case
                   deriving (Show, Typeable, Data)

data TypedefInternal = DefSimple DeclInternal
                     | DefEnum EnumDetail
                     | DefStruct StructDetail
                     | DefUnion UnionDetail
                       deriving (Show, Typeable, Data)

data Typedef = Typedef String TypedefInternal
               deriving (Show, Typeable, Data)

data ConstantDef = ConstantDef String ConstExpr
                   deriving (Show, Typeable, Data)

data Definition = DefTypedef Typedef
                | DefConstant ConstantDef
                  deriving (Show, Typeable, Data)

newtype Specification = Specification [Definition] deriving (Show, Typeable, Data)

evalConstExpr :: ConstExpr -> Integer
evalConstExpr (CEPrim p) = evalConstPrim p
evalConstExpr (CEBinExpr o c1 c2) = evalOp . fromJust . flip lookup ops $ o
  where
    evalOp op = op (evalConstExpr c1) (evalConstExpr c2)
    ops = [ (PLUS, (+)), (MINUS, (-)), (DIV, div), (MULT, (*)) 
          ]
          
evalConstPrim :: ConstPrim -> Integer
evalConstPrim (ConstLit n) = n
evalConstPrim (ConstDefRef (ConstantDef _ e)) = evalConstExpr e
evalConstPrim (ConstEnumRef _ p) = evalConstPrim p

----------------------------------------------------------------
