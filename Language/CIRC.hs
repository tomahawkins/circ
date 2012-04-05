-- | Compiler IR Compiler (CIRC): A language for specifying compiler intermediate representations.
module Language.CIRC
  ( 
  -- * CIRC Specifications
    Spec      (..)
  , Transform (..)
  , Type      (..)
  , TypeDef   (..)
  , CtorDef   (..)
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

-- | A transform is a module name, the constructor to be transformed, a list of new type definitions,
--   and the implementation (imports and code).
data Transform = Transform Name CtorName [TypeDef] [Import] (ModuleName -> Code)

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
  codeTransform (prevName, prevTypes) (Transform currName ctorName typeMods localImports code) = do
    writeFile (currName ++ ".hs") $ codeModule' currName currTypes $ Just (prevName, localImports, prevTypes, ctorName, code prevName)
    return (currName, currTypes)
    where
    filteredCtor = [ TypeDef name params [ CtorDef ctorName' args | CtorDef ctorName' args <- ctors, ctorName /= ctorName' ] | TypeDef name params ctors <- prevTypes ]
    currTypes = newTypes filteredCtor typeMods

codeModule :: ModuleName -> [String] -> TypeName -> ModuleName -> [TypeDef] -> Maybe (ModuleName, [Import], [TypeDef], CtorName, Code) -> String
codeModule initModuleName commonImports rootTypeName moduleName unsortedTypes trans = unlines $
  [ printf "module %s" moduleName
  , "  ( " ++ intercalate "\n  , " [ name ++ " (..)"| TypeDef name _ _ <- types ]
  , "  , transform"
  , "  ) where"
  , ""
  , "import Language.CIRC"
  ] ++ nub (commonImports ++ case trans of { Nothing -> []; Just (m, i, _, _, _) -> ["import qualified " ++ initModuleName, "import qualified " ++ m] ++ i}) ++
  [ ""
  ] ++ (map codeTypeDef types) ++
  case trans of
    Nothing ->
      [ printf "transform :: %s -> CIRC %s" rootTypeName rootTypeName
      , "transform = return"
      , ""
      ]
    Just (prev, _, types, ctor, code) ->
      [ printf "transform :: %s.%s -> CIRC %s" initModuleName rootTypeName rootTypeName
      , printf "transform a = %s.transform a >>= trans%s" moduleName rootTypeName
      , ""
      , codeTypeTransforms prev types ctor code
      ]
  where
  types = sortBy (compare `on` \ (TypeDef n _ _) -> n) unsortedTypes

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

codeTypeTransforms :: ModuleName -> [TypeDef] -> CtorName -> String -> String
codeTypeTransforms prev types ctor code = concatMap codeTypeTransform types
  where
  vars = map (: []) ['a' .. 'z']
  codeTypeTransform :: TypeDef -> String
  codeTypeTransform (TypeDef name _params ctors) = unlines $ -- XXX What do we do with type params?
    [ printf "trans%s :: %s.%s -> CIRC %s" name prev name name
    , printf "trans%s a = case a of" name
    , indent $ unlines $ map codeCtor ctors
    ]

  codeCtor :: CtorDef -> String
  codeCtor (CtorDef ctorName types)
    | ctorName == ctor = "\n{- Transform Begin -}\n" ++ prev ++ "." ++ drop 2 (indent code) ++ "{- Transform End -}\n"
    | otherwise        = printf "%s.%s%s -> do { %sreturn $ %s%s }" prev ctorName args (impArgs types) ctorName args
    where
    args = concat [ ' ' : v | v <- take (length types) vars ]

  impArgs :: [Type] -> Code
  impArgs types = concatMap wrapArg $ zip vars types

  wrapArg :: (Name, Type) -> Code
  wrapArg (var, typ) = printf "%s <- %s %s; " var (codeArg typ) var

  codeArg :: Type -> Code
  codeArg typ = case typ of
    t | not $ any (flip elem [ name | TypeDef name _ _ <- types ]) $ primitiveTypes t -> "return"
    T t _     -> printf "trans%s" t
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

