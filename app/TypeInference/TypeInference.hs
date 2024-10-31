{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use lambda-case" #-}
module TypeInference.TypeInference where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Parser.ParserBase (WithSimplePos (WithSimplePos))
import Parser.Types

data MaybeError a
  = Justt a
  | Error String

instance Functor MaybeError where
  fmap :: (a -> b) -> MaybeError a -> MaybeError b
  fmap f x = x >>= (\x -> pure (f x))

instance Applicative MaybeError where
  pure :: a -> MaybeError a
  pure = Justt
  (<*>) :: MaybeError (a -> b) -> MaybeError a -> MaybeError b
  f_w <*> x_w = f_w >>= (\f -> x_w >>= (\x -> pure (f x)))

instance Monad MaybeError where
  return :: a -> MaybeError a
  return = pure
  (>>=) :: MaybeError a -> (a -> MaybeError b) -> MaybeError b
  (Error msg) >>= _ = Error msg
  (Justt x) >>= f = f x

-- | This Show instance was auto-generated with the help of ChatGPT
instance Show a => Show (MaybeError a) where
  show (Justt x) = "Just " ++ show x
  show (Error err) = "Error: " ++ err

instance MonadFail MaybeError where
  fail :: String -> MaybeError a
  fail str = Error str

instance FailMessage MaybeError where
  (<?>) :: MaybeError a -> String -> MaybeError a
  maybeErr <?> msg =
    case maybeErr of
      result@(Justt _) -> result
      (Error _) -> Error msg

type Substitution = Map.Map Type Type

freeExpressionVariables :: WithSimplePos Expr -> Set.Set LabelIdentifier
freeExpressionVariables (WithSimplePos _ _ (Parentheses expression)) = freeExpressionVariables expression
freeExpressionVariables (WithSimplePos _ _ (Int _)) = Set.empty
freeExpressionVariables (WithSimplePos _ _ (Bool _)) = Set.empty
freeExpressionVariables (WithSimplePos _ _ (Label identifier)) = Set.singleton identifier
freeExpressionVariables (WithSimplePos _ _ (Application expression1 expression2)) = Set.union (freeExpressionVariables expression1) (freeExpressionVariables expression2)
freeExpressionVariables (WithSimplePos _ _ (LambdaAbstraction identifier lambdaExpr)) = Set.delete identifier (freeExpressionVariables lambdaExpr)
freeExpressionVariables (WithSimplePos _ _ (LetExpression patttern expression1 expression2)) = Set.union (freeExpressionVariables expression1) (Set.delete patttern (freeExpressionVariables expression2))

freeTypeVariables :: Type -> Set.Set Type
freeTypeVariables typ@(TypeVar _) = Set.singleton typ
freeTypeVariables typ@(FreshVar _) = Set.singleton typ
freeTypeVariables (TypeCon _) = Set.empty
freeTypeVariables (TypeArrow typ1 typ2) = Set.union (freeTypeVariables typ1) (freeTypeVariables typ2)

mostGeneralUnifier :: Type -> Type -> Maybe Substitution
mostGeneralUnifier (TypeVar name1) (TypeVar name2) | name1 == name2 = Just Map.empty
mostGeneralUnifier (FreshVar iden1) (FreshVar iden2) | iden1 == iden2 = Just Map.empty
mostGeneralUnifier (TypeCon con1) (TypeCon con2) | con1 == con2 = Just Map.empty
mostGeneralUnifier typ1@(TypeVar _) typ2 | Set.notMember typ1 (freeTypeVariables typ2) = Just $ Map.singleton typ1 typ2
mostGeneralUnifier typ1@(FreshVar _) typ2 | Set.notMember typ1 (freeTypeVariables typ2) = Just $ Map.singleton typ1 typ2
mostGeneralUnifier typ1 typ2@(TypeVar _) | Set.notMember typ2 (freeTypeVariables typ1) = Just $ Map.singleton typ2 typ1
mostGeneralUnifier typ1 typ2@(FreshVar _) | Set.notMember typ2 (freeTypeVariables typ1) = Just $ Map.singleton typ2 typ1
-- mostGeneralUnifier typ1 typ2@(TypeVar _) = Just $ Map.singleton typ2 typ1 -- TODO:verify that this may be removed
mostGeneralUnifier (TypeArrow typ1a typ1b) (TypeArrow typ2a typ2b) =
  do
    substitution1 <- mostGeneralUnifier typ1a typ2a
    substitution2 <- mostGeneralUnifier (applySubstitution substitution1 typ1b) (applySubstitution substitution1 typ2b)
    return $ composeSubstitution substitution2 substitution1
mostGeneralUnifier _ _ = Nothing

composeSubstitution :: Substitution -> Substitution -> Substitution
composeSubstitution firstSubstitution secondSubstitution =
  Map.union firstSubstitution secondSubstitution

applySubstitution :: Substitution -> Type -> Type
applySubstitution _ typ@(TypeCon _) = typ
applySubstitution substitution typ@(TypeVar _) = case Map.lookup typ substitution of
  Just x -> applySubstitution substitution x -- TODO: double check if this needs to be recursive here
  Nothing -> typ
applySubstitution substitution typ@(FreshVar _) = case Map.lookup typ substitution of
  Just x -> applySubstitution substitution x -- TODO: double check if this needs to be recursive here
  Nothing -> typ
applySubstitution substitution (TypeArrow a b) = TypeArrow (applySubstitution substitution a) (applySubstitution substitution b)

applySubstitutionToTypeEnvironment :: Substitution -> TypeEnvironment -> TypeEnvironment
applySubstitutionToTypeEnvironment substitution typeEnv = Map.map (applySubstitution substitution) typeEnv

type InferenceState = Int

newtype Inference a = Inference {runState :: InferenceState -> (MaybeError a, [Problem], InferenceState)}

runInference :: InferenceState -> Inference a -> (MaybeError a, [Problem], InferenceState)
runInference state (Inference run) = run state

instance Functor Inference where
  fmap :: (a -> b) -> Inference a -> Inference b
  fmap f x = x >>= (\x -> pure (f x))

instance Applicative Inference where
  pure :: a -> Inference a
  pure x = Inference (\state -> (Justt x, [], state))
  (<*>) :: Inference (a -> b) -> Inference a -> Inference b
  f_w <*> x_w = f_w >>= (\f -> x_w >>= (\x -> pure (f x)))

instance Monad Inference where
  return :: a -> Inference a
  return = pure
  (>>=) :: Inference a -> (a -> Inference b) -> Inference b
  (Inference runState1) >>= f =
    Inference
      ( \state ->
          let (maybeVal, problems, newState) = runState1 state
           in case maybeVal of
                Error str -> (Error str, problems, newState)
                Justt value ->
                  let Inference runState2 = f value
                      (maybeVal2, problems2, newState2) = runState2 newState
                   in (maybeVal2, problems2 ++ problems, newState2)
      )

instance MonadFail Inference where
  fail :: String -> Inference a
  fail str = Inference (\state -> (Error str, [], state))

getState :: Inference InferenceState
getState = Inference (\state -> (Justt state, [], state))

putState :: InferenceState -> Inference ()
putState state = Inference (const (Justt (), [], state))

-- takes the current FunctionName and a recover value
-- will try to do the inference given, iff it fails it will add the error to the list of problems and return the recoverVal instead reverting to the state before this section was attempted.
recover :: FunctionName -> a -> Inference a -> Inference a
recover functionName recoverVal (Inference run) =
  Inference
    ( \state ->
        let (maybeVal, problems, newState) = run state
         in case maybeVal of
              Justt _ -> (maybeVal, problems, newState)
              Error str -> (Justt recoverVal, Problem functionName str : problems, state)
    )

infixl 3 <?>

class FailMessage mon where
  (<?>) :: mon a -> String -> mon a

-- used to overwrite the error message with something more specific in case of an error.
instance FailMessage Inference where
  (<?>) :: Inference a -> String -> Inference a
  (Inference runState1) <?> msg =
    Inference
      ( \state -> case runState1 state of
          result@(Justt _, _, _) -> result
          (Error _, _, newState) -> (Error msg, [], newState)
      )

class Errorable err where
  liftError :: Maybe a -> err a
-- helper function to make working with maybe computations in do notation easier
instance Errorable Inference where
  liftError :: Maybe a -> Inference a
  liftError (Just x) = pure x
  liftError Nothing = Inference (\state -> (Error "", [], state))

instance Errorable MaybeError where
  liftError :: Maybe a -> MaybeError a
  liftError (Just x) = Justt x
  liftError Nothing = Error ""

-- get a fresh variable
freshVar :: Inference Type
freshVar = do
  counter <- getState
  putState (counter + 1)
  return $ FreshVar counter

-- to generate multiple fresh variables.
generateFreshVars :: Int -> Inference [Type]
generateFreshVars 0 = return []
generateFreshVars n = do
  var <- freshVar
  vars <- generateFreshVars (n - 1)
  return (var : vars)

generalise :: TypeEnvironment -> Type -> Type
generalise typeEnv typ = typ -- TODO: implement this properly!!!!!!!!! (supposedly this should replace fresh variables with generalised variables)

-- W type inference algorithm
typeInference :: TypeEnvironment -> WithSimplePos Expr -> Inference (Substitution, Type)
typeInference typeEnv (WithSimplePos _ _ (Parentheses expression)) = typeInference typeEnv expression
typeInference _ (WithSimplePos _ _ (Int _)) = return (Map.empty, TypeCon TypeInt)
typeInference _ (WithSimplePos _ _ (Bool _)) = return (Map.empty, TypeCon TypeBool)
typeInference typeEnv expr@(WithSimplePos start end (Label identifier)) =
  do
    foundType <-
      liftError (Map.lookup identifier typeEnv)
        <?> "Unification ERROR could not find " ++ show expr ++ " in current context at " ++ show start ++ ":" ++ show end -- TODO: proper error message that repeats offending code etc.
    let freeVars =
          Set.filter -- filter out fresh variables as they don't need to be replaced
            ( \x -> case x of
                (TypeVar _) -> True
                _ -> False
            )
            $ freeTypeVariables foundType
    freshVars <- generateFreshVars (Set.size freeVars)
    let substitution = Map.fromList $ zip (Set.toList freeVars) freshVars
    return (Map.empty, applySubstitution substitution foundType)
typeInference typeEnv (WithSimplePos _ _ (LambdaAbstraction identifier lambdaExpr)) = do
  fresh <- freshVar
  (substitution, foundType) <- typeInference (Map.insert identifier fresh (Map.delete identifier typeEnv)) lambdaExpr
  return (substitution, applySubstitution substitution (TypeArrow fresh foundType))
typeInference typeEnv (WithSimplePos _ _ (Application expression1@(WithSimplePos start1 end1 _) expression2@(WithSimplePos start2 end2 _))) = do
  (substitution1, foundType1) <- typeInference typeEnv expression1
  (substitution2, foundType2) <- typeInference (applySubstitutionToTypeEnvironment substitution1 typeEnv) expression2
  fresh <- freshVar
  substitution3 <-
    liftError (mostGeneralUnifier (applySubstitution substitution2 foundType1) (TypeArrow foundType2 fresh))
      <?> "Unification ERROR could not unify " ++ show foundType1 ++ " and  " ++ show (TypeArrow foundType2 fresh) ++ " at " ++ show start1 ++ ":" ++ show end1 ++ " and " ++ show start2 ++ ":" ++ show end2 ++ " respectively." -- TODO: proper error message that repeats offending code etc.
  return (composeSubstitution substitution3 (composeSubstitution substitution2 substitution1), applySubstitution substitution3 fresh)
typeInference typeEnv (WithSimplePos _ _ (LetExpression patttern expression1 expression2)) = do
  (substitution1, foundType1) <- typeInference typeEnv expression1
  let sigma = generalise (applySubstitutionToTypeEnvironment substitution1 typeEnv) foundType1
  (substitution2, foundType2) <- typeInference (Map.insert patttern sigma (Map.delete patttern (applySubstitutionToTypeEnvironment substitution1 typeEnv))) expression2
  return (composeSubstitution substitution2 substitution1, foundType2)