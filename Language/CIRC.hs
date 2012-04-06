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
  -- * Runtime Utilities
  , CIRC
  , evalCIRC
  , runCIRC
  , Id
  , newId
  ) where

import Control.Monad
import Control.Monad.State
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

codeModule :: ModuleName -> [String] -> TypeName -> ModuleName -> [TypeDef] -> Maybe (ModuleName, [Import], [TypeDef], CtorName, Code, [(CtorName, Code)]) -> String
codeModule initModuleName commonImports rootTypeName moduleName unsortedTypes trans = unlines $
  [ printf "module %s" moduleName
  , "  ( " ++ intercalate "\n  , " [ name ++ " (..)"| TypeDef name _ _ <- currTypes ]
  , "  , transform"
  , "  , transform'"
  , "  ) where"
  , ""
  , "import Language.CIRC"
  ] ++ nub (commonImports ++ case trans of { Nothing -> []; Just (m, i, _, _, _, _) -> ["import qualified " ++ initModuleName, "import qualified " ++ m] ++ i}) ++
  [ ""
  ] ++ (map codeTypeDef currTypes) ++
  case trans of
    Nothing ->
      [ printf "transform :: %s -> CIRC (%s, [%s])" rootTypeName rootTypeName rootTypeName
      , "transform a = return (a, [a])"
      , ""
      , printf "transform' :: %s -> %s" rootTypeName rootTypeName
      , "transform = id"
      , ""
      ]
    Just (prevName, _, prevTypes, ctor, code, backwards) ->
      [ printf "transform :: %s.%s -> CIRC (%s, [%s.%s])" initModuleName rootTypeName rootTypeName initModuleName rootTypeName
      , printf "transform a = do"
      , printf "  (a, b) <- %s.transform a" moduleName
      , printf "  a <- trans%s a" rootTypeName
      , printf "  return (a, b ++ [transform' a]"
      , printf ""
      , printf "transform' :: %s -> %s.%s" rootTypeName initModuleName rootTypeName
      , printf "transform' = %s.transform' . trans%s'" prevName rootTypeName
      , printf ""
      , codeTypeTransforms prevName prevTypes currTypes ctor code backwards
      ]
  where
  currTypes = sortBy (compare `on` \ (TypeDef n _ _) -> n) unsortedTypes

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
nextTypes old new = foldl nextType old new
  where
  nextType :: [TypeDef] -> TypeRefinement -> [TypeDef]
  nextType types refinement = case refinement of
    NewType t -> t : types
    NewCtor typeName ctorDef _ -> case match of
      [] -> error $ "Type not found: " ++ typeName 
      _ : _ : _ -> error $ "Redundent type name: " ++ typeName
      [TypeDef _ params ctors] -> TypeDef typeName params (ctorDef : ctors) : rest
      where
      (match, rest) = partition (\ (TypeDef name _ _) -> name == typeName) types
  
codeTypeTransforms :: ModuleName -> [TypeDef] -> [TypeDef] -> CtorName -> Code -> [(CtorName, Code)] -> String
codeTypeTransforms prevName prevTypes currTypes ctor code backwards = concatMap codeTypeTransform prevTypes
  where
  vars = map (: []) ['a' .. 'z']
  codeTypeTransform :: TypeDef -> String
  codeTypeTransform (TypeDef name _params ctors) = unlines $ -- XXX What do we do with type params?
    [ printf "trans%s :: %s.%s -> CIRC %s" name prevName name name
    , printf "trans%s a = case a of" name
    , indent $ unlines $ map (codeCtor ctor code prevTypes) ctors
    ]

  codeCtor :: CtorName -> Code -> [TypeDef] -> CtorDef -> String
  codeCtor targetCtorName code types (CtorDef ctorName ctorArgs)
    | ctorName == targetCtorName = "\n{- Transform Begin -}\n" ++ prevName ++ "." ++ drop 2 (indent code) ++ "{- Transform End -}\n"
    | otherwise                  = printf "%s.%s%s -> do { %sreturn $ %s%s }" prevName ctorName args (impArgs types ctorArgs) ctorName args
    where
    args = concat [ ' ' : v | v <- take (length ctorArgs) vars ]

  impArgs :: [TypeDef] -> [Type] -> Code
  impArgs typeDefs types = concatMap (wrapArg typeDefs) $ zip vars types

  wrapArg :: [TypeDef] -> (Name, Type) -> Code
  wrapArg types (var, typ) = printf "%s <- %s %s; " var (codeArg types typ) var

  codeArg :: [TypeDef] -> Type -> Code
  codeArg types typ = case typ of
    t | not $ any (flip elem [ name | TypeDef name _ _ <- types ]) $ primitiveTypes t -> "return"
    T t _     -> printf "trans%s" t
    TList  t  -> printf "mapM (%s)" $ codeArg types t 
    TMaybe t  -> printf "(\\ a -> case a of { Nothing -> return Nothing; Just a -> do { a <- %s a; return $ Just $ a } })" $ codeArg types t
    TTuple ts -> printf "(\\ (%s) -> do { %sreturn (%s) })" args (impArgs types ts) args
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

-- | The CIRC transform monad.  Used to create fresh ids.
type CIRC = State Int

-- | Evaluates a CIRC transform.
evalCIRC :: CIRC a -> Int -> a
evalCIRC = evalState

-- | Evaluates a CIRC transform, also returning the fresh next id.
runCIRC :: CIRC a -> Int -> (a, Int)
runCIRC = runState

-- | Identifiers.
type Id = String

-- | Produces a fresh id.
newId :: CIRC Id
newId = do
  id <- get
  put $ id + 1
  return $ "__" ++ show id

