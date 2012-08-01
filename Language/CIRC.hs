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
  , ModuleName
  , CtorName
  , TypeName
  , TypeParam
  , Code
  , Import
  , t
  , indent
  -- * CIRC Compilation
  , circ
  ) where

import Control.Monad
import Data.Function
import Data.List
import System.Directory
import System.IO
import Text.Printf

-- | A specification is the initial type module name, the initial transform module name, the root type, the initial type definitions, and a list of transforms.
data Spec = Spec ModuleName ModuleName TypeName [TypeDef] [Transform]

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
data Transform = Transform ModuleName [Import] [Import] [(CtorName, ModuleName -> Code)] [TypeRefinement]

-- | An unparameterized type.
t :: String -> Type
t n = T n []

-- | Compiles a CIRC spec.
circ :: Spec -> IO ()
circ (Spec initTypeModuleName initTransModuleName rootTypeName typeDefsUnsorted transforms) = do
  foldM_ codeTransform (initTransModuleName, typeDefs) transforms
  where
  typeDefs = sortTypeDefs typeDefsUnsorted

  codeTransform :: (Name, [TypeDef]) -> Transform -> IO (Name, [TypeDef])
  codeTransform (prevModuleName, prevTypeDefs) (Transform moduleName typeImports transImports removedCtors typeRefinements) = do
    maybeWriteFile (moduleName ++ ".hs") $ codeTypeModule moduleName typeImports typeDefs
    maybeWriteFile (moduleName ++ "Trans.hs") $ codeTransModule
      initTypeModuleName
      rootTypeName
      prevModuleName
      prevTypeDefs
      moduleName
      transImports
      typeDefs
      [ (name, code prevModuleName) | (name, code) <- removedCtors ]
      [ (ctorName, transCode prevModuleName) | NewCtor _ (CtorDef ctorName _) transCode <- typeRefinements ]
    return (moduleName, typeDefs)
    where
    filteredCtor = [ TypeDef name params [ CtorDef ctorName args | CtorDef ctorName args <- ctors, notElem ctorName $ fst $ unzip removedCtors ] | TypeDef name params ctors <- prevTypeDefs ]
    typeDefs = sortTypeDefs $ filterRelevantTypes rootTypeName $ nextTypes filteredCtor typeRefinements

-- | Write out a file if the file doesn't exist or is different.  Doesn't bump the timestamp for Makefile-like build systems.
maybeWriteFile :: FilePath -> String -> IO ()
maybeWriteFile file contents = do
  a <- doesFileExist file
  if not a then writeFile file contents else do
    f <- openFile file ReadMode
    contents' <- hGetContents f
    if contents' == contents
      then do
        hClose f
        return ()
      else do
        hClose f
        writeFile file contents

-- | Sort a list of TypeDefs by type name.
sortTypeDefs :: [TypeDef] -> [TypeDef]
sortTypeDefs = sortBy (compare `on` \ (TypeDef n _ _) -> n)

-- | Code the module that contains the IR datatype definitions.
codeTypeModule :: ModuleName -> [Import] -> [TypeDef] -> String
codeTypeModule moduleName imports typeDefs = unlines $
  [ printf "module %s" moduleName
  , "  ( " ++ intercalate "\n  , " [ name ++ " (..)"| TypeDef name _ _ <- typeDefs ]
  , "  ) where"
  , ""
  ] ++ nub (["import Language.CIRC.Runtime"] ++ imports) ++ [""] ++ map codeTypeDef typeDefs
  where
  codeTypeDef :: TypeDef -> String
  codeTypeDef (TypeDef name params ctors) = "data " ++ name ++ " " ++ intercalate " " params ++ "\n  = " ++ 
    intercalate "\n  | " [ name ++ replicate (m - length name) ' ' ++ " " ++ intercalate " " (map codeType args) | CtorDef name args <- ctors' ] ++ "\n"
    where
    ctors' = sortBy (compare `on` \ (CtorDef n _) -> n) ctors
    m = maximum [ length n | CtorDef n _ <- ctors ]
  
  codeType :: Type -> String
  codeType a = case a of
    T name []     ->        name
    T name params -> "(" ++ name ++ intercalate " " (map codeType params) ++ ")"
    TList  a      -> "[" ++ codeType a ++ "]"
    TMaybe a      -> "(Maybe " ++ codeType a ++ ")"
    TTuple a      -> "(" ++ intercalate ", " (map codeType a) ++ ")"

-- | Code the module that contains the IR transformations.
codeTransModule :: ModuleName -> TypeName -> ModuleName -> [TypeDef] -> ModuleName -> [Import] -> [TypeDef] -> [(CtorName, Code)] -> [(CtorName, Code)] -> String
codeTransModule initModuleName rootTypeName prevModuleName prevTypeDefs moduleName imports typeDefs transCode backwardTransCode = unlines $
  [ printf "module %sTrans" moduleName
  , "  ( transform"
  , "  , transform'"
  , "  ) where"
  , ""
  ] ++ nub (
    [ "import Language.CIRC.Runtime"
    , "import qualified " ++ initModuleName
    , "import qualified " ++ prevModuleName
    , "import qualified " ++ prevModuleName ++ "Trans"
    , "import " ++ moduleName
    ] ++ imports) ++
  [ printf ""
  , printf "transform :: %s.%s -> CIRC (%s, [%s.%s])" initModuleName rootTypeName rootTypeName initModuleName rootTypeName
  , printf "transform a = do"
  , printf "  (a, b) <- %sTrans.transform a" prevModuleName
  , printf "  a <- trans%s a" rootTypeName
  , printf "  c <- transform' a"
  , printf "  return (a, b ++ [c])"
  , printf ""
  , printf "transform' :: %s -> CIRC %s.%s" rootTypeName initModuleName rootTypeName
  , printf "transform' a = trans%s' a >>= %sTrans.transform'" rootTypeName prevModuleName
  , printf ""
  , codeTypeTransforms prevModuleName prevTypeDefs typeDefs transCode backwardTransCode
  , printf ""
  ]

-- | Codes the type transform function.
codeTypeTransforms :: ModuleName -> [TypeDef] -> [TypeDef] -> [(CtorName, Code)] -> [(CtorName, Code)] -> String
codeTypeTransforms prevName prevTypes currTypes forwardTrans backwardTrans =
  concatMap (codeTypeTransform prevTypes forwardTrans  (\ t -> "trans" ++ t)        qualified id) [ t | t@(TypeDef n _ _) <- prevTypes, elem n $ map typeDefName currTypes ] ++
  concatMap (codeTypeTransform currTypes backwardTrans (\ t -> "trans" ++ t ++ "'") id qualified) [ t | t@(TypeDef n _ _) <- currTypes, elem n $ map typeDefName prevTypes ]
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

-- | Get rid of types that are not relevant to the root type.
filterRelevantTypes :: TypeName -> [TypeDef] -> [TypeDef]
filterRelevantTypes rootTypeName types = [ t | t@(TypeDef n _ _) <- types, elem n required ]
  where
  typeDeps :: TypeName -> [TypeName]
  typeDeps name = nub $ concat [ concat [ concatMap primitiveTypes t | CtorDef _ t <- ctors ] | TypeDef n _ ctors <- types, n == name ]

  required = next ([], [rootTypeName])

  next :: ([TypeName], [TypeName]) -> [TypeName]
  next (sofar, remaining) = case remaining of
    [] -> sofar
    a : rest
      | elem a sofar -> next (sofar, rest)
      | otherwise    -> next (a : sofar, rest ++ typeDeps a)

