{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module TypeInference.TypeInferenceUtil where

import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Parser.ParserBase
import Parser.Types
import TypeInference.TypeInference

getDependencies :: Section -> Set.Set LabelIdentifier
getDependencies (FunctionDefinition _ labels expr) = foldl (\set (WithSimplePos _ _ identifier) -> Set.delete identifier set) (freeExpressionVariables expr) labels
getDependencies (FunctionType _ _) = Set.empty

-- TODO: handle infinite loops!
gatherDepths :: [Section] -> Map.Map FunctionName Int
gatherDepths sections =
  let calculateDebth :: Section -> Int
      calculateDebth section =
        Set.foldl
          ( \acc dependency -> max acc $ case Map.lookup dependency result of
              Just depth -> depth
              Nothing -> -1
          )
          (-1)
          (getDependencies section)
          + 1

      result =
        List.foldl
          ( \map section -> case section of
              (FunctionDefinition name _ _) -> Map.insert name (calculateDebth section) map
              (FunctionType _ _) -> map
          )
          Map.empty
          sections
   in result

{-  -2 is reserved for FunctionType
    -1 is for names for which the depth could not be found -}
sortSectionsOnDependencies :: [Section] -> [(Section, Int)]
sortSectionsOnDependencies sections =
  let depths = gatherDepths sections
   in List.sortOn snd $
        List.map
          ( \section -> case section of
              (FunctionDefinition name _ _) -> case Map.lookup name depths of
                Just x -> (section, x)
                Nothing -> (section, -1)
              (FunctionType _ _) -> (section, -2)
          )
          sections

inferTypeEnvironment :: TypeEnvironment -> [Section] -> MaybeError ([String], TypeEnvironment)
inferTypeEnvironment typeEnv sections =
  let orderedSections = List.map fst $ sortSectionsOnDependencies sections
      processSection :: ([String], TypeEnvironment) -> Section -> Inference ([String], TypeEnvironment)
      processSection (errors, typeEnvIn) (FunctionType name (WithSimplePos _ _ typeIn)) = pure (errors, Map.insert name typeIn typeEnvIn)
      processSection (errors, typeEnvIn) (FunctionDefinition name functionArguments expr) =
        if Map.member name typeEnvIn
          then pure ([], typeEnvIn) -- there was a user given type definition TODO: maybe do type checking here
          else
            let x =
                  runInference
                    0
                    ( do
                        freshVars <- generateFreshVars (List.length functionArguments)
                        let inputTypes = zip (List.map (\(WithSimplePos _ _ x) -> x) functionArguments) freshVars
                        (substitution, inferredReturnType) <- typeInference (Map.union (Map.fromList inputTypes) typeEnvIn) expr
                        let inferredType = applySubstitution substitution (List.foldr (\(_, typ) val -> TypeArrow typ val) inferredReturnType inputTypes)
                        return $ Map.insert name inferredType typeEnvIn
                    )
             in case x of
                  (Justt typeEnvOut, newState) -> do
                    put newState
                    pure (errors, typeEnvOut)
                  (Error str, _) -> pure (errors ++ [str], typeEnvIn)
   in fst $ runInference 0 $ Monad.foldM processSection ([], typeEnv) orderedSections