-- | Compiler IR Compiler (CIRC): A language for specifying compiler intermediate representations.
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
  , t
  , circ
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

type Name      = String
type CtorName  = String
type TypeName  = String
type TypeParam = String
type Code      = Name -> String
type Import    = String

-- | A type expression.
data Type = T TypeName [Type] | TList Type | TMaybe Type | TFun Type Type | TTuple [Type]

-- | A type definition is a name, a list of type parameters, and a list of constructor definitions.
data TypeDef = TypeDef TypeName [TypeParam] [CtorDef]

-- | A constructor definition is a name and a list of type arguments.
data CtorDef = CtorDef CtorName [Type]

-- | A transform is a module name, the constructor to be transformed, a list of new type definitions,
--   and the implementation (imports and code).
data Transform = Transform Name CtorName [TypeDef] [Import] Code  -- name, removed ctor, new types or ctors, transform code.

-- | An unparameterized type.
t :: String -> Type
t n = T n []

-- | Compiles a CIRC spec.
circ :: Spec -> IO ()
circ (Spec initName imports root types transforms) = do
  writeFile (initName ++ ".hs") $ codeModule initName types Nothing
  foldM_ codeTransform (initName, types) transforms
  where
  codeTransform :: (Name, [TypeDef]) -> Transform -> IO (Name, [TypeDef])
  codeTransform (name', types) trans@(Transform name ctorName types' _ _) = do
    writeFile (name ++ ".hs") $ codeModule name types'' $ Just (name', trans)
    return (name, types'')
    where
    filteredCtor = [ TypeDef name params [ CtorDef ctorName' args | CtorDef ctorName' args <- ctors, ctorName /= ctorName' ] | TypeDef name params ctors <- types ]
    types'' = newTypes filteredCtor types'

  codeModule :: Name -> [TypeDef] -> Maybe (Name, Transform) -> String
  codeModule name types trans = unlines $
    [ printf "module %s" name
    , "  ( " ++ intercalate "\n  , " [ name ++ " (..)"| TypeDef name _ _ <- types' ]
    , "  , transform"
    , "  ) where"
    , ""
    , "import Language.CIRC"
    ] ++ (case trans of { Nothing -> []; Just (m, _) -> nub ["import qualified " ++ initName, "import qualified " ++ m]}) ++ imports ++
    [ ""
    ] ++ (map codeTypeDef types') ++
    case trans of
      Nothing ->
        [ printf "transform :: %s -> CIRC %s" root root
	, "transform = return"
	, ""
	]
      Just (name, trans) ->
        [ printf "transform :: %s.%s -> CIRC %s" initName root root
	, printf "transform a = %s.transform a >>= trans%s" name root
	, ""
	]
    where
    types' = sortBy (compare `on` \ (TypeDef n _ _) -> n) types

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
  TFun   a b    -> "(" ++ codeType a ++ " -> " ++ codeType b ++ ")"
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

