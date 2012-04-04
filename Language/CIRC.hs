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
  , circ
  ) where

import Control.Monad
import Data.List
import Text.Printf

data Spec = Spec Name [TypeDef] [Transform] -- Module name with initial types (root type is first one defined).

type Name      = String
type CtorName  = String
type TypeName  = String
type TypeParam = String
type Code      = String

data Type = Type TypeName [Type] | TypeList Type | TypeMaybe Type | TypeFun Type Type

data TypeDef = TypeDef TypeName [TypeParam] [CtorDef]

data CtorDef = CtorDef CtorName [Type]

data Transform = Transform Name CtorName [TypeDef] Code  -- name, removed ctor, new types or ctors, transform code.

circ :: Spec -> IO ()
circ (Spec name types transforms) = do
  writeFile (name ++ ".hs") $ codeTypes name types
  foldM_ codeTransform types transforms

codeTransform :: [TypeDef] -> Transform -> IO [TypeDef]
codeTransform types (Transform name ctorName types' code) = do
  writeFile (name ++ ".hs") $ codeTypes name types''
  return types''
  where
  filteredCtor = [ TypeDef name params [ CtorDef ctorName' args | CtorDef ctorName' args <- ctors, ctorName /= ctorName' ] | TypeDef name params ctors <- types ]
  types'' = newTypes filteredCtor types'

codeTypes :: Name -> [TypeDef] -> String
codeTypes name types = unlines
  [ printf "module %s" name
  , "  ( " ++ intercalate "\n  , " [ name ++ " (..)"| TypeDef name _ _ <- types ]
  , "  )"
  , ""
  ] ++ unlines (map codeTypeDef types)

codeTypeDef :: TypeDef -> String
codeTypeDef (TypeDef name params ctors) = "data " ++ name ++ " " ++ intercalate " " params ++ "\n  = " ++ 
  intercalate "\n  | " [ name ++ " " ++ intercalate " " (map codeType args) | CtorDef name args <- ctors ] ++ "\n"

codeType :: Type -> String
codeType a = case a of
  Type name []     ->        name
  Type name params -> "(" ++ name ++ intercalate " " (map codeType params) ++ ")"
  TypeList  a      -> "[" ++ codeType a ++ "]"
  TypeMaybe a      -> "(Maybe " ++ codeType a ++ ")"
  TypeFun   a b    -> "(" ++ codeType a ++ " -> " ++ codeType b ++ ")"

newTypes :: [TypeDef] -> [TypeDef] -> [TypeDef]
newTypes old new = foldl newType old new

newType :: [TypeDef] -> TypeDef -> [TypeDef]
newType old new@(TypeDef name params ctors) = case match of
  [] -> new : old
  [TypeDef _ params' ctors']
    | params /= params' -> error $ "Type " ++ name ++ " parameter mismatch."
    | otherwise         -> TypeDef name params (ctors' ++ ctors) : rest
  _ -> error $ "Duplicate type names: " ++ name
  where
  (match, rest) = partition (\ (TypeDef name' _ _) -> name' == name) old


