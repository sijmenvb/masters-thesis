{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use const" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Suggestions.Suggestions where

import Data.Foldable
import Data.IntMap (insert)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Tree (Tree (subForest))
import Data.Void
import Debug.Trace (trace)
import GHC.Base (TyCon (TyCon))
import Lexer.Tokens (Token (..), TokenInfo (TokenInfo, token_type), recreateOriginalShow, showTokenInfoWithPosition)
import Parser.Parser (buildLambdaExpression)
import Parser.ParserBase
import Parser.Types
import Suggestions.TokenDifference (Action (..), ExtendedTokens, generateActions, recreateOriginalWithDifferencesShow, sectionToSuggestion)
import Text.Megaparsec (ParseErrorBundle, Stream (Token), between, errorBundlePretty)
import TypeInference.TypeInference
  ( Errorable (..),
    FailMessage (..),
    MaybeError (..),
    Substitution,
    applySubstitution,
    applySubstitutionToTypeEnvironment,
    mostGeneralUnifier,
  )

indexedMap :: (Int -> a -> b) -> [a] -> [b]
indexedMap f xs = zipWith f [0 ..] xs

getSectionName :: [TokenInfo] -> String
getSectionName list = case list of
  (TokenInfo (Name str) _ _ _ : _) -> str
  _ -> "ERROR!"

matchProblems :: [[TokenInfo]] -> [Problem] -> [([TokenInfo], Problem)]
matchProblems tokens problems =
  let isTypeAnnotation :: [TokenInfo] -> Bool
      isTypeAnnotation list = case list of
        (_ : TokenInfo DoubleColon _ _ _ : _) -> True
        _ -> False

      tokensWithoutTypes = foldl (\currentMap x -> if isTypeAnnotation x then currentMap else Map.insert (getSectionName x) x currentMap) Map.empty tokens
   in mapMaybe
        ( \problem@(Problem name _) -> do
            tok <- Map.lookup name tokensWithoutTypes
            return (tok, problem)
        )
        problems

getParseProblems :: [Either (ParseErrorBundle MyStream Void) (WithSimplePos Section)] -> [[TokenInfo]] -> [([TokenInfo], Problem)]
getParseProblems parsedMaybeSections sections =
  foldl
    ( \list (eith, tokens) -> case eith of
        Left err -> (tokens, Problem (getSectionName tokens) (errorBundlePretty err)) : list
        Right _ -> list
    )
    []
    $ zip parsedMaybeSections sections

dropTypeArguments :: [WithSimplePos LabelIdentifier] -> TypeEnvironment -> Type -> MaybeError (Type, TypeEnvironment)
dropTypeArguments arguments typeEnv typ =
  case (arguments, typ) of
    ([], t) -> Justt (t, typeEnv)
    ((WithSimplePos _ _ identifier) : xs, TypeArrow a b) -> dropTypeArguments xs (Map.insert identifier a typeEnv) b
    ((WithSimplePos _ _ identifier) : xs, _) -> Error $ "Cannot take an argument from the type " ++ show typ ++ " to assign to " ++ show identifier ++ "!"

type FreshVariableCounter = Int

-- See Note [Suggestions/Suggestions/generateSuggestion]
generateSuggestion :: FreshVariableCounter -> Map.Map String Type -> [TokenInfo] -> MaybeError ([ExtendedTokens], String, String, Type, Int)
generateSuggestion state typeEnv tokens =
  let functionName = getSectionName tokens
      getArguments :: [TokenInfo] -> [WithSimplePos LabelIdentifier]
      getArguments list = case list of
        (TokenInfo EqualsSign _ _ _ : _) -> []
        (TokenInfo (Name name) _ start end : xs) -> WithSimplePos start end name : getArguments xs
        (TokenInfo tok _ start end : xs) -> WithSimplePos start end ("weird token " ++ show tok) : getArguments xs
        _ -> []

      arguments :: [WithSimplePos LabelIdentifier]
      arguments = getArguments (tail tokens) -- TODO: make sure this errors instead of crashing
      argumentTypeVars :: TypeEnvironment
      argumentTypeVars =
        case Map.lookup functionName typeEnv of
          -- in case no type was given just make the arguments free variables.
          _ -> Map.fromList $ indexedMap (\index (WithSimplePos _ _ name) -> (name, FreshVar $ state + index)) arguments
          Just expectedFunctionType -> Map.empty

      expressionTokens = (tail . dropWhile (\tokenInfo -> token_type tokenInfo /= EqualsSign)) tokens

      buildReturnType :: TypeEnvironment -> [WithSimplePos LabelIdentifier] -> Type -> MaybeError Type
      buildReturnType typeEnv args typ =
        let go argsIn = case argsIn of
              [] -> Justt typ
              (WithSimplePos _ _ identifier) : xs ->
                do
                  argType <- liftError $ Map.lookup identifier typeEnv
                  resultType <- go xs
                  return $ TypeArrow argType resultType
         in go args
   in do
        (typeGoal, typeEnvWithArgs) <- case Map.lookup functionName typeEnv of
          Just x -> dropTypeArguments arguments typeEnv x
          Nothing -> Justt (FreshVar $ state + length argumentTypeVars, Map.union argumentTypeVars typeEnv)
        let builder =
              do
                setTypeEnvironment (typeEnvWithArgs)
                generateExpressionSuggestion typeGoal Nothing []
        (suggestions, builderState) <- runSugesstionBuilder (state + length argumentTypeVars + 1) builder expressionTokens
        (expr, typ, _) <- liftError (findFirstMatch typeGoal suggestions) <?> "Could not generate a suggestion that matches the goal: " ++ show typeGoal
        let section = FunctionDefinition functionName arguments expr
        let (expectedTokens, _) = sectionToSuggestion section
        let tokenList = map (\(TokenInfo tok _ _ _) -> tok) tokens
        let actionTokens = generateActions expectedTokens tokenList
        finalType <- buildReturnType (getEnvironmentFromState builderState) arguments typ
        -- trace (show (getEnvironmentFromState builderState)) $
        return
          ( expectedTokens,
            recreateOriginalShow $
              mapMaybe
                ( \action -> case action of
                    Keep a -> Just a
                    Add a -> Just a
                    Remove _ -> Nothing
                )
                actionTokens,
            recreateOriginalWithDifferencesShow actionTokens,
            finalType,
            getBranchCounterFromState builderState
          )

-- <?> "could not generate a suggestion for " ++ show name ++ " " ++ show arguments ++ " " ++ show expressionTokens

data SuggestionBuilderState = State
  { getTokensFromState :: [TokenInfo],
    getEnvironmentFromState :: TypeEnvironment,
    getFreshVarCounterFromState :: Int,
    getBranchCounterFromState :: Int
  }
  deriving (Show)

newtype SuggestionBuilder a = RunState (SuggestionBuilderState -> (MaybeError a, SuggestionBuilderState))

instance Functor SuggestionBuilder where
  fmap :: (a -> b) -> SuggestionBuilder a -> SuggestionBuilder b
  fmap f ma = do
    a <- ma
    return $ f a

instance Applicative SuggestionBuilder where
  pure :: a -> SuggestionBuilder a
  pure a = RunState (\state -> (Justt a, state))

  (<*>) :: SuggestionBuilder (a -> b) -> SuggestionBuilder a -> SuggestionBuilder b
  fm <*> am = do
    f <- fm
    a <- am
    return $ f a

instance Monad SuggestionBuilder where
  return :: a -> SuggestionBuilder a
  return = pure
  (>>=) :: SuggestionBuilder a -> (a -> SuggestionBuilder b) -> SuggestionBuilder b
  (RunState run1) >>= f =
    RunState
      ( \state1 ->
          let (ma, newState) = run1 state1
           in case ma of
                Justt a ->
                  let (RunState run2) = f a
                   in run2 newState
                (Error str) -> (Error str, newState)
      )

instance MonadFail SuggestionBuilder where
  fail :: String -> SuggestionBuilder a
  fail str = RunState (\state -> (Error str, state))

instance FailMessage SuggestionBuilder where
  (<?>) :: SuggestionBuilder a -> String -> SuggestionBuilder a
  (RunState run) <?> str =
    RunState
      ( \state ->
          let (ma, newState) = run state
           in ( ma <?> str,
                newState
              )
      )

instance Errorable SuggestionBuilder where
  liftError :: Maybe a -> SuggestionBuilder a
  liftError (Just a) = pure a
  liftError Nothing = fail ""

runSugesstionBuilder :: Int -> SuggestionBuilder a -> [TokenInfo] -> MaybeError (a, SuggestionBuilderState)
runSugesstionBuilder freshVarCounter (RunState run) tokens =
  let (ma, state) = run State {getTokensFromState = tokens, getEnvironmentFromState = Map.empty, getFreshVarCounterFromState = freshVarCounter, getBranchCounterFromState = 0}
   in do
        a <- ma
        return (a, state)

getSuggestionBuilderState :: SuggestionBuilder SuggestionBuilderState
getSuggestionBuilderState =
  RunState (\state -> (Justt state, state))

setSuggestionBuilderState :: SuggestionBuilderState -> SuggestionBuilder ()
setSuggestionBuilderState state = RunState (\_ -> (Justt (), state))

getTypeEnvironment :: SuggestionBuilder TypeEnvironment
getTypeEnvironment = getEnvironmentFromState <$> getSuggestionBuilderState

setTypeEnvironment :: TypeEnvironment -> SuggestionBuilder ()
setTypeEnvironment env = do
  state <- getSuggestionBuilderState
  setSuggestionBuilderState state {getEnvironmentFromState = env}

getFreshVarCounter :: SuggestionBuilder Int
getFreshVarCounter = getFreshVarCounterFromState <$> getSuggestionBuilderState

setFreshVarCounter :: Int -> SuggestionBuilder ()
setFreshVarCounter counter = do
  state <- getSuggestionBuilderState
  setSuggestionBuilderState state {getFreshVarCounterFromState = counter}

getFreshVar :: SuggestionBuilder Type
getFreshVar = do
  freshVarCounter <- getFreshVarCounter
  setFreshVarCounter $ freshVarCounter + 1
  return $ FreshVar freshVarCounter

{- getBranchCounter :: SuggestionBuilder Int
getBranchCounter = getBranchCounterFromState <$> getSuggestionBuilderState

setBranchCounter :: Int -> SuggestionBuilder ()
setBranchCounter branchCounter = do
  state <- getSuggestionBuilderState
  setSuggestionBuilderState state {getBranchCounterFromState = branchCounter}

countBranch :: SuggestionBuilder ()
countBranch = do
  count <- getBranchCounter
  setBranchCounter (count + 1) -}

applySubstitutionToSuggestionBuilder :: Substitution -> SuggestionBuilder ()
applySubstitutionToSuggestionBuilder sub =
  do
    currentEnv <- getTypeEnvironment
    let newEnv = Map.insert ("added " ++ show sub) (TypeCon TypeBool) $ applySubstitutionToTypeEnvironment sub currentEnv
    setTypeEnvironment newEnv

getTokens :: SuggestionBuilder [TokenInfo]
getTokens =
  getTokensFromState <$> getSuggestionBuilderState

popToken :: SuggestionBuilder TokenInfo
popToken =
  RunState
    ( \state -> case getTokensFromState state of
        [] -> (Error "no items to pop", state)
        (x : xs) -> (Justt x, state {getTokensFromState = xs})
    )

hasTokens :: SuggestionBuilder Bool
hasTokens =
  RunState
    ( \state -> case getTokensFromState state of
        [] -> (Justt False, state)
        _ -> (Justt True, state)
    )

-- try :: SuggestionBuilder a -> SuggestionBuilder a -> SuggestionBuilder a
try :: Show a => SuggestionBuilder a -> SuggestionBuilder a -> SuggestionBuilder a
try (RunState run1) (RunState run2) =
  RunState
    ( \state -> case run1 state of
        res@(Justt _, _) -> res
        (Error str, _) ->
          -- trace str $
          run2 (state {getBranchCounterFromState = getBranchCounterFromState state + 1})
    )

getToken :: SuggestionBuilder TokenInfo -> SuggestionBuilder TokenInfo
getToken x = do
  hasTok <- hasTokens
  if hasTok
    then popToken
    else x

cycleExpression :: WithSimplePos Expr -> WithSimplePos Expr
cycleExpression exprIn =
  let expressionToArguments :: WithSimplePos Expr -> ([(WithSimplePos Expr, (Int, Int), (Int, Int))], WithSimplePos Expr)
      expressionToArguments expr =
        case expr of
          (WithSimplePos start end (Application expr1 expr2)) -> let (list, function) = expressionToArguments expr1 in (list ++ [(expr2, start, end)], function)
          _ -> ([], expr)

      buildExpressionFromArguments arguments function =
        case reverse arguments of
          [] -> function
          (x, start, end) : xs -> WithSimplePos start end (Application (buildExpressionFromArguments xs function) x)
   in case expressionToArguments exprIn of
        (firstArg : restOfArgs, function) -> buildExpressionFromArguments (restOfArgs ++ [firstArg]) function
        _ -> exprIn

-- Candidate holds suggested expressions and their type, it also holds the state of the program at the point it was generated for backtracking purposes.
type Candidate = (WithSimplePos Expr, Type, SuggestionBuilderState)

findFirstMatch :: Type -> [Candidate] -> Maybe Candidate
findFirstMatch _ [] = Nothing
findFirstMatch goal ((canExpr, canTyp, canState) : list) = case mostGeneralUnifier canTyp goal of
  Nothing -> findFirstMatch goal list
  Just substitution -> Just (canExpr, applySubstitution substitution canTyp, canState)

-- takes a target type, a maybe in case  we are currently building a function and a list of the candidates found thus far.
generateExpressionSuggestion :: Type -> Maybe Type -> [Candidate] -> SuggestionBuilder [Candidate]
generateExpressionSuggestion goal currentProcessType accumulator =
  let getExpr :: SuggestionBuilder Candidate
      getExpr =
        do
          tok <- getToken (fail $ "Not Enough tokens to satisfy goal:\n" ++ show goal ++ "\n!")
          case tok of
            (TokenInfo (Name name) _ start end) ->
              do
                typeEnv <- getTypeEnvironment
                typ <- liftError (Map.lookup name typeEnv) <?> "could not find " ++ name ++ " in type environment"
                currentState <- getSuggestionBuilderState
                return (WithSimplePos start end (Label name), typ, currentState)
            (TokenInfo (Number num) _ start end) ->
              case mostGeneralUnifier (TypeCon TypeInt) goal of
                Nothing -> fail $ "Int does not match " ++ show goal
                Just sub -> do
                  applySubstitutionToSuggestionBuilder sub
                  currentState <- getSuggestionBuilderState
                  return (WithSimplePos start end (Int num), TypeCon TypeInt, currentState)
            (TokenInfo TrueToken _ start end) ->
              case mostGeneralUnifier (TypeCon TypeBool) goal of
                Nothing -> fail $ "Bool does not match " ++ show goal
                Just sub -> do
                  applySubstitutionToSuggestionBuilder sub
                  currentState <- getSuggestionBuilderState
                  return (WithSimplePos start end (Bool True), TypeCon TypeBool, currentState)
            (TokenInfo FalseToken _ start end) ->
              case mostGeneralUnifier (TypeCon TypeBool) goal of
                Nothing -> fail $ "Bool does not match " ++ show goal
                Just sub -> do
                  applySubstitutionToSuggestionBuilder sub
                  currentState <- getSuggestionBuilderState
                  return (WithSimplePos start end (Bool False), TypeCon TypeBool, currentState)
            (TokenInfo Lambda _ start end) -> processLambda start
            _ -> getExpr

      processLambda start = do
        currentTypeEnv <- getTypeEnvironment
        let (goalArguments, goalReturnType) = typeToArguments goal
        arguments <- case goalReturnType of
          (FreshVar _) ->
            -- in case the return type of the goal is a free variable we do not know how many arguments should be consumed.
            getArgumentsFromTokens (-1)
          _ ->
            -- in case the goal's return type is some constant we know how many arguments at maximum it will take
            getArgumentsFromTokens (length goalArguments)
        consumeTokenIfExists RArrow -- only consume the -> if it exists this helps in cases where the wrong symbol is used or the arrow is forgotten (and we know by the goal how many arguments we need)
        unconsumedArguments <- addArgumentsToTypeEnvironment goalArguments arguments
        let expressionGoal = buildTypeFromArguments unconsumedArguments goalReturnType
        candidates <- generateExpressionSuggestion expressionGoal Nothing []
        let go candidateList = case candidateList of
              [] -> fail $ "could not generate an expression for this lambda stating at " ++ show start
              (expr, typ, env) : otherCandidates -> do
                setSuggestionBuilderState env -- if we partially apply we need to revert the state.
                newTypeEnv <- getTypeEnvironment
                let typeArguments = mapMaybe (\(TokenInfo (Name name) _ _ _) -> Map.lookup name newTypeEnv) arguments
                let finalType = buildTypeFromArguments typeArguments typ
                case mostGeneralUnifier finalType goal of
                    Nothing -> go otherCandidates --if lambda type does not match goal try partially applied version
                    Just sub -> do
                      finalExpr <- buildLambdaExpression start (map liftTokenInfoToSimplePos arguments) expr
                      revertTypeEnvironment arguments currentTypeEnv -- we revert the type environment to before we added the arguments of the lambda to the typeEnv.
                      currentState <- getSuggestionBuilderState
                      return (finalExpr, finalType, currentState)
        go candidates

      -- adds the candidate to the accumulator. (regardless of the goal) --TODO: performance: maybe only if the function could end up in the goal instead of always.
      -- if it matches the goal the substitution will also be applied to the candidates type and state.
      -- it will revert to before this substitution so one can continue taking arguments. 
      addToAccumulator :: [Candidate] -> Type -> Candidate -> SuggestionBuilder [Candidate]
      addToAccumulator accumulator goal candidate@(candidateExpr, candidateType, candidateState) = case mostGeneralUnifier candidateType goal of
        Nothing -> return $ candidate : accumulator
        Just substitution -> do
          applySubstitutionToSuggestionBuilder substitution
          let finalCandidateType = applySubstitution substitution candidateType
          finalCandidateState <- getSuggestionBuilderState
          setSuggestionBuilderState candidateState
          return $ (candidateExpr, finalCandidateType, finalCandidateState) : accumulator
   in case currentProcessType of
        Nothing ->
          try
            ( do
                nextArg@(expr, nextArgType, _) <- getExpr
                case nextArgType of
                  (TypeArrow _ _) -> do
                    newAccumulator <- addToAccumulator accumulator goal nextArg
                    generateExpressionSuggestion goal (Just nextArgType) newAccumulator
                  -- TODO: filter on matching the goal here???
                  _ -> addToAccumulator accumulator goal nextArg
            )
            (return accumulator)
        Just functionType@(TypeArrow argTyp retTyp) ->
          let (previousExpr, _, _) : _ = accumulator -- will never be an empty list. This is the reason we must be able to return candidates that do not match the goal (yet).
              plugInArgument :: [(WithSimplePos Expr, Type, SuggestionBuilderState)] -> SuggestionBuilder [Candidate]
              plugInArgument candidates = case candidates of
                (expr, nextArgType, state) : xs -> do
                  setSuggestionBuilderState state

                  case mostGeneralUnifier nextArgType argTyp of
                    Nothing ->
                      -- if the greedy version doesn't fit try the slightly less greedy result.
                      plugInArgument xs
                    Just sub ->
                      try
                        ( do
                            applySubstitutionToSuggestionBuilder sub
                            let retTypWithSub = applySubstitution sub retTyp
                            currentState <- getSuggestionBuilderState
                            newAccumulator <- addToAccumulator accumulator goal (buildApplication previousExpr expr, retTypWithSub, currentState)
                            res <- generateExpressionSuggestion goal (Just retTypWithSub) newAccumulator
                            -- trace ("fits:" ++ show (map (\(a, b, _) -> (a, b)) res))
                            return res
                        )
                        ( -- if the generateExpressionSuggestion above fails still see if the less greedy version works
                          plugInArgument xs
                        )
                _ ->
                  -- no arguments fit
                  fail "no arguments fit"

              swapArguments :: [(WithSimplePos Expr, Type, SuggestionBuilderState)] -> SuggestionBuilder [Candidate]
              swapArguments candidates = case candidates of
                (foundExpr, nextArgType, state) : xs -> do
                  setSuggestionBuilderState state
                  case typeToArguments functionType of
                    (typeArguments, returnType) ->
                      case firstMatchToFront nextArgType typeArguments of
                        Nothing -> swapArguments xs -- see if a partial application fits
                        Just (foundIndex, fittingArgumentsSequence) -> do
                          let reorderedType = buildTypeFromArguments (tail fittingArgumentsSequence) returnType
                          newArgsAndTypes <- generateExpressionSuggestion goal (Just reorderedType) accumulator

                          -- The foundIndex needs to be adjusted to also incorporate the number of arguments we have already given at this point
                          -- firstMatchToFront only looks at the current function type, not the whole. (it needs to look art just the remainder to avoid swapping to the same type twice)
                          -- see Note [Suggestions/Suggestions/note on indexing for swapping]
                          let (previousExpr, _, _) : _ = accumulator -- will never be an empty list.
                          let currentIndex = length (snd (expressionToArguments previousExpr))
                          let insertionIndex = currentIndex + foundIndex

                          let result =
                                mapMaybe
                                  ( \(expr, typ, state) -> do
                                      newExpr <- insertIntoExpressionAtIndex insertionIndex foundExpr expr
                                      return (newExpr, typ, state)
                                  )
                                  (take (length newArgsAndTypes - length accumulator) newArgsAndTypes) -- we take to make sure we only insert on new suggestions
                          return $ result ++ accumulator
                _ ->
                  -- no arguments fit
                  fail $ "could not swap the arguments of " ++ show functionType
           in do
                try
                  ( do
                      nextArgumentCandidates <- generateExpressionSuggestion argTyp Nothing []
                      plugInArgument nextArgumentCandidates
                  )
                  ( try
                      ( do
                          freshVar <- getFreshVar
                          nextArgumentCandidates <- generateExpressionSuggestion freshVar Nothing []
                          swapArguments nextArgumentCandidates
                      )
                      (return accumulator)
                  )
        Just _ -> return accumulator

getArgumentsFromTokens :: Int -> SuggestionBuilder [TokenInfo]
getArgumentsFromTokens 0 = return []
getArgumentsFromTokens limit = do
  currentState <- getSuggestionBuilderState
  token <- popToken
  case token of
    (TokenInfo (Name name) _ start end) -> do
      list <- getArgumentsFromTokens (limit - 1)
      return $ token : list
    _ -> do
      setSuggestionBuilderState currentState -- un-consume the token
      return []

consumeTokenIfExists :: Lexer.Tokens.Token -> SuggestionBuilder ()
consumeTokenIfExists target = do
  currentState <- getSuggestionBuilderState
  token <- popToken
  case token of
    (TokenInfo foundToken _ _ _) | foundToken == target -> return ()
    _ ->
      do
        setSuggestionBuilderState currentState -- un-consume the token
        return ()

-- takes a list of types and tokens and adds the tokens to the type enviornment.
-- if there are types it a token will get it assigned otherwise it gets a free variable.
-- will Return the types that were not assigned.
addArgumentsToTypeEnvironment :: [Type] -> [TokenInfo] -> SuggestionBuilder [Type]
addArgumentsToTypeEnvironment types [] = return types
addArgumentsToTypeEnvironment (typ : restOfTypes) (x : xs) = do
  -- if there is a type given use it
  addLabelToTypeEnvironment typ x
  addArgumentsToTypeEnvironment restOfTypes xs
addArgumentsToTypeEnvironment [] (x : xs) = do
  -- if there is no type given use a fresh variable
  addLabelToTypeEnvironmentAsFreshVar x
  addArgumentsToTypeEnvironment [] xs

addLabelToTypeEnvironment :: Type -> TokenInfo -> SuggestionBuilder ()
addLabelToTypeEnvironment typ (TokenInfo (Name name) _ start end) = do
  typeEnv <- getTypeEnvironment
  setTypeEnvironment $ Map.insert name typ typeEnv
addLabelToTypeEnvironment _ _ = fail "internal ERROR addLabelToTypeEnvironmentAsFreshVar was not given a Name token!"

addLabelToTypeEnvironmentAsFreshVar :: TokenInfo -> SuggestionBuilder ()
addLabelToTypeEnvironmentAsFreshVar (TokenInfo (Name name) _ start end) = do
  typeEnv <- getTypeEnvironment
  freshVar <- getFreshVar
  setTypeEnvironment $ Map.insert name freshVar typeEnv
addLabelToTypeEnvironmentAsFreshVar _ = fail "internal ERROR addLabelToTypeEnvironmentAsFreshVar was not given a Name token!"

revertTypeEnvironment :: [TokenInfo] -> TypeEnvironment -> SuggestionBuilder ()
revertTypeEnvironment tokens oldTypeEnvironment = do 
  newTypeEnvironment <- getTypeEnvironment
  let revert [] typeEnv = typeEnv
      revert (TokenInfo (Name name) _ _ _: restOfTokens) typeEnv = case Map.lookup name oldTypeEnvironment of
        Nothing -> revert restOfTokens $ Map.delete name typeEnv
        Just typ -> revert restOfTokens $ Map.insert name typ typeEnv
  setTypeEnvironment $ revert tokens newTypeEnvironment

-- moves the first match with the first argument to the front, reordering the list. and giving the index to undo the swapping later
firstMatchToFront :: Type -> [Type] -> Maybe (Int, [Type])
firstMatchToFront foundType goals =
  let go (x : xs) index =
        case mostGeneralUnifier foundType x of
          Just _ -> Just (index, x, xs)
          Nothing -> do
            (a, b, c) <- go xs (index + 1)
            return (a, b, x : c)
      go [] _ = Nothing
   in do
        (index, match, rest) <- go goals 0
        return (index, match : rest)

insertIntoExpressionAtIndex :: Int -> WithSimplePos Expr -> WithSimplePos Expr -> Maybe (WithSimplePos Expr)
insertIntoExpressionAtIndex indexIn expr originalExpr =
  let (function, arguments) = expressionToArguments originalExpr
      insertAtPos :: Int -> a -> [a] -> Maybe [a]
      insertAtPos 0 element list = Just (element : list)
      insertAtPos index element (x : list) =
        do
          result <- insertAtPos (index - 1) element list
          return (x : result)
      insertAtPos _ _ _ = Nothing
   in do
        newArguments <- insertAtPos indexIn expr arguments
        return $ buildExpressionFromArguments function newArguments
