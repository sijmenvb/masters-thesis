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
  let calculateDepth :: Section -> Int
      calculateDepth section =
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
              (FunctionDefinition name _ _) -> Map.insert name (calculateDepth section) map
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

inferTypeEnvironment :: TypeEnvironment -> [Section] -> (TypeEnvironment, [Problem])
inferTypeEnvironment typeEnv sections =
  let orderedSections :: [Section]
      orderedSections = List.map fst $ sortSectionsOnDependencies sections

      processSection :: TypeEnvironment -> Section -> Inference TypeEnvironment
      processSection typeEnvIn (FunctionType name (WithSimplePos _ _ typeIn)) =
        recover
          name
          typeEnvIn
          $ if Map.member name typeEnvIn
            then fail $ name ++ " has multiple type annotations!"
            else pure (Map.insert name typeIn typeEnvIn)
      processSection typeEnvIn (FunctionDefinition name functionArguments expr) =
        case Map.lookup name typeEnvIn of
          Just foundType -> do
            state <- getState
            _ <-
              recover
                name
                typeEnvIn
                ( do
                    freshVars <- generateFreshVars (List.length functionArguments)
                    let inputTypes = zip (List.map (\(WithSimplePos _ _ x) -> x) functionArguments) freshVars
                    (substitution, inferredReturnType) <- typeInference (Map.union (Map.fromList inputTypes) typeEnvIn) expr
                    let inferredType = applySubstitution substitution (List.foldr (\(_, typ) val -> TypeArrow typ val) inferredReturnType inputTypes)
                    let mgu = mostGeneralUnifier inferredType foundType -- TODO: check if the order of the arguments is correct here
                    case mgu of
                      Just _ -> return typeEnvIn
                      Nothing -> fail $ "the inferred type of " ++ name ++ " is " ++ show inferredType ++ " but the type annotation says it should be " ++ show foundType ++ " these do not match!" -- TODO: add the conflicting types.
                )
            putState state
            pure typeEnvIn
          Nothing ->
            recover
              name
              typeEnvIn
              ( do
                  freshVars <- generateFreshVars (List.length functionArguments)
                  let inputTypes = zip (List.map (\(WithSimplePos _ _ x) -> x) functionArguments) freshVars
                  (substitution, inferredReturnType) <- typeInference (Map.union (Map.fromList inputTypes) typeEnvIn) expr
                  let inferredType = applySubstitution substitution (List.foldr (\(_, typ) val -> TypeArrow typ val) inferredReturnType inputTypes)
                  return $ Map.insert name inferredType typeEnvIn
              )

      (maybeEnv, problems) = runInference 0 $ Monad.foldM processSection typeEnv orderedSections
   in case maybeEnv of
        Justt finalTypeEnv -> (finalTypeEnv, problems)
        Error str -> (typeEnv, Problem "ERROR!!!!" str : problems) -- if this happens you probably forgot a recover somewhere.