{-# LANGUAGE DeriveDataTypeable #-}

module Data.XDR.AST
    ( Constant (..)
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
    , ImportSpecification (..)
    ) where

import Data.Generics
import Data.Map (Map)
import System.Path

----------------------------------------------------------------

data Constant = ConstLit Integer
                deriving (Show, Typeable, Data)

data DeclInternal = DeclSimple Type
                  | DeclArray Type Constant
                  | DeclVarArray Type (Maybe Constant)
                  | DeclOpaque Constant
                  | DeclVarOpaque (Maybe Constant)
                  | DeclString (Maybe Constant)
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

newtype EnumDetail = EnumDetail [(String, Constant)]
    deriving (Show, Typeable, Data)

newtype StructDetail = StructDetail [Decl]
    deriving (Show, Typeable, Data)

data UnionDetail = UnionDetail Decl [(Constant, Decl)] (Maybe Decl) -- selector, cases, default case
                   deriving (Show, Typeable, Data)

data TypedefInternal = DefSimple DeclInternal
                     | DefEnum EnumDetail
                     | DefStruct StructDetail
                     | DefUnion UnionDetail
                       deriving (Show, Typeable, Data)

data Typedef = Typedef String TypedefInternal
               deriving (Show, Typeable, Data)

data ConstantDef = ConstantDef String Constant
                   deriving (Show, Typeable, Data)

data Definition = DefTypedef Typedef
                | DefConstant ConstantDef
                  deriving (Show, Typeable, Data)

newtype Specification = Specification [Definition] 
                      deriving (Show, Typeable, Data)
                               
data ImportSpecification = ImportSpecification { importedSpecs :: Map AbsFile ImportSpecification
                                               , spec :: Specification
                                               } deriving (Show)

----------------------------------------------------------------
