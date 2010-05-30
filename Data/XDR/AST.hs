{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleInstances #-}
-- | Data types for defining the abstract syntax tree of an XDR file.
module Data.XDR.AST
    ( 
      -- * Constant expressions
      ConstExpr (..)
    , BinOp (..)
    , UnOp (..)
    , evalConstExpr
    
      -- * XDR types
    , Specification (..)
    , Module (..)
    , Definition (..)
    , Typedef (..)
    , ConstantDef (..)
    , TypedefInternal (..)
    , DeclInternal (..)
    , EnumDetail (..)
    , StructDetail (..)
    , UnionDetail (..)
    , Decl (..)
    , Type (..)
    ) where

import Data.Bits
import Data.Generics
import Data.Map (Map)
import Data.Maybe
import System.Path

-- | Constants are represented as symbolic expressions to allow the code
--   generators to produce more tractable code.  Utility functions can
--   evaluate these expressions.  A primary expression is just a reference to
--   a literal, global constant or previously defined enum element.
data ConstExpr = ConstLit Integer
               | ConstRef ConstantDef
               | ConstBinExpr BinOp ConstExpr ConstExpr
               | ConstUnExpr UnOp ConstExpr
               deriving (Show, Typeable, Data)

data BinOp = MUL | DIV | MOD | ADD | SUB | SHL | SHR | AND | XOR | OR
             deriving (Eq, Show, Typeable, Data)

data UnOp = NEG | NOT
            deriving (Eq, Show, Typeable, Data)

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

newtype EnumDetail = EnumDetail [ConstantDef]
    deriving (Show, Typeable, Data)

newtype StructDetail = StructDetail [Decl]
    deriving (Show, Typeable, Data)

-- | A union consists of a selector type, a set of cases and possibly
-- a default case.
data UnionDetail = UnionDetail Decl [(ConstExpr, Decl)] (Maybe Decl)
                   deriving (Show, Typeable, Data)

data TypedefInternal = DefSimple DeclInternal
                     | DefEnum EnumDetail
                     | DefStruct StructDetail
                     | DefUnion UnionDetail
                       deriving (Show, Typeable, Data)

-- | A typedef associates a name with a type.
data Typedef = Typedef String TypedefInternal
               deriving (Show, Typeable, Data)

data ConstantDef = ConstantDef String ConstExpr
                   deriving (Show, Typeable, Data)

-- | A definition either introduces a new type, or defines a constant.
data Definition = DefTypedef Typedef
                | DefConstant ConstantDef
                  deriving (Show, Typeable, Data)

-- | The module name as list of elements
data Module = Module [String]
            deriving (Show, Eq, Ord, Typeable, Data)

data Specification = Specification { 
  moduleName :: Module

  -- | A map of other xdr files that have been imported.  Empty if the
  -- 'Data.XDR.Parser.Imports' language option was not enabled.
  , imports :: Map Module Specification
  
  -- | The data type definitions
  , defs :: [Definition]
  } deriving (Show)

-- | Evaluates a constant expression
evalConstExpr :: ConstExpr -> Integer
evalConstExpr (ConstLit n) = n
evalConstExpr (ConstRef (ConstantDef _ e)) = evalConstExpr e
evalConstExpr (ConstBinExpr o c1 c2) = evalOp . fromJust . flip lookup ops $ o
  where
    evalOp op = op (evalConstExpr c1) (evalConstExpr c2)
    ops = [ (MUL, (*))
          , (DIV, div)
          , (MOD, mod)
          , (ADD, (+))
          , (SUB, (-))
          , (SHL, flip $ flip shiftL . fromIntegral)
          , (SHR, flip $ flip shiftR . fromIntegral)
          , (AND, (.&.))
          , (XOR, xor)
          , (OR, (.|.))
          ]
evalConstExpr (ConstUnExpr NEG c) = negate . evalConstExpr $ c
evalConstExpr (ConstUnExpr NOT c) = complement . evalConstExpr $ c
