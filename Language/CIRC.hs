module Language.CIRC
  ( Spec      (..)
  , Transform (..)
  , Type      (..)
  , TypeDef   (..)
  , CtorDef   (..)
  , Name
  , CtorName
  , TypeName
  , TypeParam
  , Code
  ) where

data Spec = Spec [TypeDef] [Transform]

type Name      = String
type CtorName  = String
type TypeName  = String
type TypeParam = String
type Code      = String

data Type = Type TypeName [Type] | TypeList Type | TypeMaybe Type | TypeFun Type Type

data TypeDef = TypeDef TypeName [TypeParam] [CtorDef]

data CtorDef = CtorDef CtorName [Type]

data Transform = Transform Name CtorName [TypeDef] Code  -- name, removed ctor, new types or ctors, transform code.

