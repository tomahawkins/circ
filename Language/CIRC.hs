-- | Compiler IR Compiler (CIRC): A language for specifying compiler intermediate representations.
module Language.CIRC
  ( 
  -- * CIRC Specifications
    Spec           (..)
  , Transform      (..)
  , Type           (..)
  , TypeDef        (..)
  , CtorDef        (..)
  , TypeRefinement (..)
  , Name
  , CtorName
  , TypeName
  , TypeParam
  , Code
  , t
  , indent
  -- * CIRC Compilation
  , circ
  ) where

import Control.Monad
import Data.Function
import Data.List
import Text.Printf

-- | A specification is a module name for the initial type, common imports, the root type, the initial type definitions, and a list of transforms.
data Spec = Spec Name [Import] TypeName [TypeDef] [Transform]

type Name       = String
type ModuleName = String
type CtorName   = String
type TypeName   = String
type TypeParam  = String
type Code       = String
type Import     = String

-- | A type expression.
data Type = T TypeName [Type] | TList Type | TMaybe Type | TTuple [Type]

-- | A type definition is a name, a list of type parameters, and a list of constructor definitions.
data TypeDef = TypeDef TypeName [TypeParam] [CtorDef]

-- | A constructor definition is a name and a list of type arguments.
data CtorDef = CtorDef CtorName [Type]

-- | A type refinement.
data TypeRefinement
  = NewCtor TypeName CtorDef (ModuleName -> Code)
  | NewType TypeDef

-- | A transform is a module name, the constructor to be transformed, a list of new type definitions,
--   and the implementation (imports and code).
data Transform = Transform ModuleName [Import] CtorName (ModuleName -> Code) [TypeRefinement]

-- | An unparameterized type.
t :: String -> Type
t n = T n []

-- | Compiles a CIRC spec.
circ :: Spec -> IO ()
circ (Spec initModuleName commonImports rootTypeName types transforms) = do
  writeFile (initModuleName ++ ".hs") $ codeModule' initModuleName types Nothing
  foldM_ codeTransform (initModuleName, types) transforms
  where
  codeModule' = codeModule initModuleName commonImports rootTypeName
  codeTransform :: (Name, [TypeDef]) -> Transform -> IO (Name, [TypeDef])
  codeTransform (prevName, prevTypes) (Transform currName localImports ctorName code typeMods) = do
    writeFile (currName ++ ".hs") $ codeModule' currName currTypes $ Just (prevName, localImports, prevTypes, ctorName, code prevName, [ (ctor, code prevName)| NewCtor _ (CtorDef ctor _) code <- typeMods ])
    return (currName, currTypes)
    where
    filteredCtor = [ TypeDef name params [ CtorDef ctorName' args | CtorDef ctorName' args <- ctors, ctorName /= ctorName' ] | TypeDef name params ctors <- prevTypes ]
    currTypes = nextTypes filteredCtor typeMods

sortTypeDefs :: [TypeDef] -> [TypeDef]
sortTypeDefs = sortBy (compare `on` \ (TypeDef n _ _) -> n)

codeModule :: ModuleName -> [String] -> TypeName -> ModuleName -> [TypeDef] -> Maybe (ModuleName, [Import], [TypeDef], CtorName, Code, [(CtorName, Code)]) -> String
codeModule initModuleName commonImports rootTypeName moduleName unsortedTypes trans = unlines $
  [ printf "module %s" moduleName
  , "  ( " ++ intercalate "\n  , " [ name ++ " (..)"| TypeDef name _ _ <- currTypes ]
  , "  , transform"
  , "  , transform'"
  , "  ) where"
  , ""
  , "import Language.CIRC.Runtime"
  ] ++ nub (commonImports ++ case trans of { Nothing -> []; Just (m, i, _, _, _, _) -> ["import qualified " ++ initModuleName, "import qualified " ++ m] ++ i}) ++
  [ ""
  ] ++ (map codeTypeDef currTypes) ++
  case trans of
    Nothing ->
      [ printf "transform :: %s -> CIRC (%s, [%s])" rootTypeName rootTypeName rootTypeName
      , "transform a = return (a, [a])"
      , ""
      , printf "transform' :: %s -> CIRC %s" rootTypeName rootTypeName
      , "transform' = return"
      , ""
      ]
    Just (prevName, _, prevTypes, ctor, code, backwards) ->
      [ printf "transform :: %s.%s -> CIRC (%s, [%s.%s])" initModuleName rootTypeName rootTypeName initModuleName rootTypeName
      , printf "transform a = do"
      , printf "  (a, b) <- %s.transform a" prevName
      , printf "  a <- trans%s a" rootTypeName
      , printf "  c <- transform' a"
      , printf "  return (a, b ++ [c])"
      , printf ""
      , printf "transform' :: %s -> CIRC %s.%s" rootTypeName initModuleName rootTypeName
      , printf "transform' a = trans%s' a >>= %s.transform'" rootTypeName prevName
      , printf ""
      , codeTypeTransforms prevName prevTypes currTypes (ctor, code) backwards
      ]
  where
  currTypes = sortTypeDefs unsortedTypes

codeTypeDef :: TypeDef -> String
codeTypeDef (TypeDef name params ctors) = "data " ++ name ++ " " ++ intercalate " " params ++ "\n  = " ++ 
  intercalate "\n  | " [ name ++ " " ++ intercalate " " (map codeType args) | CtorDef name args <- ctors' ] ++ "\n"
  where
  ctors' = sortBy (compare `on` \ (CtorDef n _) -> n) ctors

codeType :: Type -> String
codeType a = case a of
  T name []     ->        name
  T name params -> "(" ++ name ++ intercalate " " (map codeType params) ++ ")"
  TList  a      -> "[" ++ codeType a ++ "]"
  TMaybe a      -> "(Maybe " ++ codeType a ++ ")"
  TTuple a      -> "(" ++ intercalate ", " (map codeType a) ++ ")"

-- | Computes the next type definitions given a list of type definitions and a list of type refinements.
nextTypes :: [TypeDef] -> [TypeRefinement] -> [TypeDef]
nextTypes old new = sortTypeDefs $ foldl nextType old new
  where
  nextType :: [TypeDef] -> TypeRefinement -> [TypeDef]  -- XXX Need to filter out types that are no longer used, i.e. reachable from root type.
  nextType types refinement = case refinement of
    NewType t -> t : types
    NewCtor typeName ctorDef _ -> case match of
      [] -> error $ "Type not found: " ++ typeName 
      _ : _ : _ -> error $ "Redundent type name: " ++ typeName
      [TypeDef _ params ctors] -> TypeDef typeName params (ctorDef : ctors) : rest
      where
      (match, rest) = partition (\ (TypeDef name _ _) -> name == typeName) types

codeTypeTransforms :: ModuleName -> [TypeDef] -> [TypeDef] -> (CtorName, Code) -> [(CtorName, Code)] -> String
codeTypeTransforms prevName prevTypes currTypes forwardTrans backwardTrans =
  concatMap (codeTypeTransform prevTypes [forwardTrans] (\ t -> "trans" ++ t)        qualified id) prevTypes ++
  concatMap (codeTypeTransform currTypes backwardTrans  (\ t -> "trans" ++ t ++ "'") id qualified) [ t | t@(TypeDef n _ _) <- currTypes, elem n $ map typeDefName prevTypes ]
  where
  typeDefName (TypeDef n _ _) = n
  qualified :: String -> String
  qualified a = prevName ++ "." ++ a
  vars = map (: []) ['a' .. 'z']
  codeTypeTransform :: [TypeDef] -> [(CtorName, Code)] -> (TypeName -> String) -> (CtorName -> String) -> (CtorName -> String) -> TypeDef -> String
  codeTypeTransform fromTypes transforms transName from to (TypeDef typeName _params ctors) = unlines $ -- XXX What do we do with type params?
    [ transName typeName ++ " :: " ++ from typeName ++ " -> CIRC " ++ to typeName
    , transName typeName ++ " a = case a of"
    , indent $ unlines $ map codeCtor ctors
    ]
    where
    codeCtor :: CtorDef -> String
    codeCtor (CtorDef ctorName ctorArgs) = case lookup ctorName transforms of
      Nothing   -> from ctorName ++ args ++ " -> do { " ++ impArgs ctorArgs ++ "return $ " ++ to ctorName ++ args ++ " }"
      Just code -> "\n{- Transform Begin -}\n" ++ (from $ drop 2 $ indent code) ++ "{- Transform End -}\n" 
      where
      args = concat [ ' ' : v | v <- take (length ctorArgs) vars ]

    impArgs :: [Type] -> Code
    impArgs types = concatMap wrapArg $ zip vars types

    wrapArg :: (Name, Type) -> Code
    wrapArg (var, typ) = printf "%s <- %s %s; " var (codeArg typ) var

    codeArg :: Type -> Code
    codeArg typ = case typ of
      t | not $ any (flip elem [ name | TypeDef name _ _ <- fromTypes ]) $ primitiveTypes t -> "return"
      T t _     -> transName t
      TList  t  -> printf "mapM (%s)" $ codeArg t 
      TMaybe t  -> printf "(\\ a -> case a of { Nothing -> return Nothing; Just a -> do { a <- %s a; return $ Just $ a } })" $ codeArg t
      TTuple ts -> printf "(\\ (%s) -> do { %sreturn (%s) })" args (impArgs ts) args
        where
        args = intercalate ", " $ take (length ts) vars

-- | Returns a list of names of all primitive types used in a type.
primitiveTypes :: Type -> [TypeName]
primitiveTypes a = case a of
  T n _     -> [n]
  TList t   -> primitiveTypes t
  TMaybe t  -> primitiveTypes t
  TTuple ts -> concatMap primitiveTypes ts

-- | Indents code with 2 spaces.
indent :: String -> String
indent = unlines . map ("  " ++) . lines

