{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use const" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Replace case with maybe" #-}

module Suggestions.Suggestions where

import Control.Exception (throw)
import Data.Array (Ix (range))
import qualified Data.Bifunctor
import Data.Foldable
import Data.Function (on)
import Data.IntMap (insert)
import Data.List (groupBy, sortBy, sortOn)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Tree (Tree (subForest))
import Data.Void
import Debug.Trace (trace)
import GHC.Base (Alternative (empty), TyCon (TyCon))
import GHC.ExecutionStack (Location (functionName))
import Lexer.Tokens (Token (..), TokenInfo (TokenInfo, start_pos, token_type), recreateOriginalShow, showTokenInfoWithPosition)
import Parser.Parser (buildLambdaExpression)
import Parser.ParserBase
  ( MyStream,
    WithSimplePos (WithSimplePos),
    liftTokenInfoToSimplePos,
  )
import qualified Parser.ParserBase as Lexer.Tokens.TokenInfo
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
    generateFreshVars,
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

      expressionTokens = case dropWhile (\tokenInfo -> token_type tokenInfo /= EqualsSign) tokens of
        [] -> []
        exprTokens -> tail exprTokens

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
        (suggestions, builderState) <- runSuggestionBuilder (state + length argumentTypeVars + 1) builder expressionTokens

        (expr, typ, _) <- trace ("Debug suggestions:" ++ show suggestions ++ show tokens) $ liftError (findFirstMatch typeGoal suggestions) <?> "Could not generate a suggestion that matches the goal: " ++ show typeGoal
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
    -- getLocalDefinitionTokensFromState :: [[TokenInfo]]
  }
  deriving (Show)

newtype SuggestionBuilder a = SuggestionBuilder {runState :: SuggestionBuilderState -> (MaybeError a, SuggestionBuilderState)}

instance Functor SuggestionBuilder where
  fmap :: (a -> b) -> SuggestionBuilder a -> SuggestionBuilder b
  fmap f ma = do
    a <- ma
    return $ f a

instance Applicative SuggestionBuilder where
  pure :: a -> SuggestionBuilder a
  pure a = SuggestionBuilder {runState = \state -> (Justt a, state)}

  (<*>) :: SuggestionBuilder (a -> b) -> SuggestionBuilder a -> SuggestionBuilder b
  fm <*> am = do
    f <- fm
    a <- am
    return $ f a

instance Monad SuggestionBuilder where
  return :: a -> SuggestionBuilder a
  return = pure
  (>>=) :: SuggestionBuilder a -> (a -> SuggestionBuilder b) -> SuggestionBuilder b
  run1 >>= f =
    SuggestionBuilder
      { runState =
          \state1 ->
            let (ma, newState) = runState run1 state1
             in case ma of
                  Justt a ->
                    let run2 = f a
                     in runState run2 newState
                  (Error str) -> (Error str, newState)
      }

instance MonadFail SuggestionBuilder where
  fail :: String -> SuggestionBuilder a
  fail str = SuggestionBuilder {runState = \state -> (Error str, state)}

instance FailMessage SuggestionBuilder where
  (<?>) :: SuggestionBuilder a -> String -> SuggestionBuilder a
  (run) <?> str =
    SuggestionBuilder
      { runState =
          \state ->
            let (ma, newState) = runState run state
             in ( ma <?> str,
                  newState
                )
      }

instance Errorable SuggestionBuilder where
  liftError :: Maybe a -> SuggestionBuilder a
  liftError (Just a) = pure a
  liftError Nothing = fail ""

runSuggestionBuilder :: Int -> SuggestionBuilder a -> [TokenInfo] -> MaybeError (a, SuggestionBuilderState)
runSuggestionBuilder freshVarCounter run tokens =
  let (ma, state) =
        runState
          run
          State
            { getTokensFromState = tokens,
              getEnvironmentFromState = Map.empty,
              getFreshVarCounterFromState = freshVarCounter,
              getBranchCounterFromState = 0
              -- getLocalDefinitionTokensFromState = []
            }
   in do
        a <- ma
        return (a, state)

getSuggestionBuilderState :: SuggestionBuilder SuggestionBuilderState
getSuggestionBuilderState =
  SuggestionBuilder {runState = \state -> (Justt state, state)}

setSuggestionBuilderState :: SuggestionBuilderState -> SuggestionBuilder ()
setSuggestionBuilderState state = SuggestionBuilder {runState = \_ -> (Justt (), state)}

getTypeEnvironment :: SuggestionBuilder TypeEnvironment
getTypeEnvironment = getEnvironmentFromState <$> getSuggestionBuilderState

setTypeEnvironment :: TypeEnvironment -> SuggestionBuilder ()
setTypeEnvironment env = do
  state <- getSuggestionBuilderState
  setSuggestionBuilderState state {getEnvironmentFromState = env}

{- getLocalDefinitionTokens :: SuggestionBuilder [[TokenInfo]]
getLocalDefinitionTokens = getLocalDefinitionTokensFromState <$> getSuggestionBuilderState

setLocalDefinitionTokens :: [[TokenInfo]] -> SuggestionBuilder ()
setLocalDefinitionTokens env = do
  state <- getSuggestionBuilderState
  setSuggestionBuilderState state {getLocalDefinitionTokensFromState = env}
 -}
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

getFreshVars :: Int -> SuggestionBuilder [Type]
getFreshVars 0 = return []
getFreshVars num = do
  res <- getFreshVar
  rest <- getFreshVars (num - 1)
  return $ res : rest

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

setTokens :: [TokenInfo] -> SuggestionBuilder ()
setTokens tokens = do
  state <- getSuggestionBuilderState
  setSuggestionBuilderState $ state {getTokensFromState = tokens}

popToken :: SuggestionBuilder TokenInfo
popToken =
  SuggestionBuilder
    { runState =
        \state -> case getTokensFromState state of
          [] -> (Error "no items to pop", state)
          (x : xs) -> (Justt x, state {getTokensFromState = xs})
    }

hasTokens :: SuggestionBuilder Bool
hasTokens =
  SuggestionBuilder
    { runState =
        \state -> case getTokensFromState state of
          [] -> (Justt False, state)
          _ -> (Justt True, state)
    }

try :: SuggestionBuilder a -> SuggestionBuilder a -> SuggestionBuilder a
try run1 run2 =
  SuggestionBuilder
    { runState =
        \state -> case runState run1 state of
          res@(Justt _, _) -> res
          (Error str, _) ->
            -- trace str $
            runState run2 (state {getBranchCounterFromState = getBranchCounterFromState state + 1})
    }

removeWhitespace :: [TokenInfo] -> [TokenInfo]
removeWhitespace [] = []
removeWhitespace ((TokenInfo Indent _ _ _) : xs) = removeWhitespace xs
removeWhitespace ((TokenInfo Dedent _ _ _) : xs) = removeWhitespace xs
removeWhitespace ((TokenInfo Newline _ _ _) : xs) = removeWhitespace xs
removeWhitespace ((TokenInfo NewlineAfterComment _ _ _) : xs) = removeWhitespace xs
removeWhitespace ((TokenInfo (Comment _) _ _ _) : xs) = removeWhitespace xs
removeWhitespace list = list

consumeWhitespace :: SuggestionBuilder ()
consumeWhitespace = do
  tokens <- getTokens
  setTokens $ removeWhitespace tokens

getToken :: SuggestionBuilder TokenInfo -> SuggestionBuilder TokenInfo
getToken x = do
  hasTok <- hasTokens
  if hasTok
    then popToken
    else x

-- | takes an string or an error in case the list is empty.
--
-- Tries all suggestion builders will return the first success.
--
-- gives the first error if ALL fail.
firstSuccessful :: String -> [SuggestionBuilder a] -> SuggestionBuilder a -- (auto-generated with the help of ChatGPT (needed small fixes))
firstSuccessful emptyListErrorStr [] = fail emptyListErrorStr
firstSuccessful _ (f : fs) =
  SuggestionBuilder
    { runState = \s ->
        let (result, newState) = runState f s
         in case result of
              Justt _ -> (result, newState)
              Error _ -> trace ("###@$@#$@#$@#$@#$@ found error" ++ show (length fs)) $ tryRest result fs s
    }
  where
    tryRest firstFailure [] originalState = (firstFailure, originalState)
    tryRest firstFailure (g : gs) originalState =
      case runState g originalState of
        (Justt val, newState) -> (Justt val, newState)
        (Error _, _) -> tryRest firstFailure gs originalState

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

dropTypeArguments2 :: [LabelIdentifier] -> Type -> SuggestionBuilder (Type, TypeEnvironment)
dropTypeArguments2 arguments typ = do
  typeEnv <- getTypeEnvironment
  case (arguments, typ) of
    ([], t) -> return (t, typeEnv)
    (_, FreshVar _) ->
      do
        freshVar1 <- getFreshVar
        freshVar2 <- getFreshVar
        let newtyp = TypeArrow freshVar1 freshVar2
        let sub = Map.insert typ newtyp Map.empty
        applySubstitutionToSuggestionBuilder sub
        dropTypeArguments2 arguments newtyp
    (identifier : xs, TypeArrow a b) -> do
      setTypeEnvironment (Map.insert identifier a typeEnv)
      dropTypeArguments2 xs b
    (identifier : xs, _) -> fail $ "Cannot take an argument from the type " ++ show typ ++ " to assign to " ++ show identifier ++ "!"

generateFunctionSuggestion :: Type -> SuggestionBuilder (Pattern, [LabelIdentifier], WithSimplePos Expr, Type)
generateFunctionSuggestion goal =
  let getNextName = do
        tok <- getToken (fail $ "Not Enough tokens to satisfy goal:\n" ++ show goal ++ "\n!")
        case tok of
          (TokenInfo (Name name) _ start end) -> return name
          _ -> fail $ "name is not next, found: " ++ show tok ++ " instead"

      many :: SuggestionBuilder a -> SuggestionBuilder [a]
      many x =
        try
          ( do
              res <- x
              rest <- many x
              return $ res : rest
          )
          (return [])

      buildReturnType :: [LabelIdentifier] -> Type -> SuggestionBuilder Type
      buildReturnType args exprTyp =
        let go typeEnv argsIn = case argsIn of
              [] -> return exprTyp
              identifier : xs ->
                do
                  argType <- liftError $ Map.lookup identifier typeEnv
                  resultType <- go typeEnv xs
                  return $ TypeArrow argType resultType
         in do
              typeEnv <- getTypeEnvironment
              go typeEnv args
   in do
        functionName <- getNextName
        arguments <- many getNextName
        trace ("function name in let:" ++ functionName) $ consumeWhitespace
        consumeTokenIfExists EqualsSign

        freshVars <- getFreshVars (length arguments)
        let pairs = zip arguments freshVars
        startingTypeEnv <- trace ("fresh vars:" ++ show freshVars) $ getTypeEnvironment
        let typeEnvWithFreshVars = foldl (\typeEnv (argumentName, argumentType) -> Map.insert argumentName argumentType (Map.delete argumentName typeEnv)) startingTypeEnv pairs
        setTypeEnvironment typeEnvWithFreshVars

        (expressionGoal, updatedTypeEnv) <- trace ("goal:" ++ show goal) $ dropTypeArguments2 arguments goal
        trace ("expressionGoal:" ++ show expressionGoal) $ setTypeEnvironment updatedTypeEnv

        functionBodyCandidates <- generateExpressionSuggestion expressionGoal Nothing []

        -- add back function variables
        -- trace ("functionBodyCandidates:" ++ show functionBodyCandidates) $
        findBestCandidate
          goal
          functionBodyCandidates
          (buildReturnType arguments)
          ( \expr typ sub -> do
              return (functionName, arguments, expr, typ)
          )

-- findBestCandidate :: Type -> [Candidate] -> SuggestionBuilder Candidate
findBestCandidate :: Type -> [Candidate] -> (Type -> SuggestionBuilder Type) -> (WithSimplePos Expr -> Type -> Substitution -> SuggestionBuilder b) -> SuggestionBuilder b
findBestCandidate goal candidates typeTransform suggestionBuilderToTry =
  let plugInArgument list =
        case list of
          (expr, nextArgType, state) : xs -> do
            setSuggestionBuilderState state
            currentTypeEnv <- getTypeEnvironment
            typeToCompare <- typeTransform nextArgType
            case mostGeneralUnifier typeToCompare goal of
              Nothing ->
                -- if the greedy version doesn't fit try the slightly less greedy result.
                plugInArgument xs
              Just sub ->
                try
                  (suggestionBuilderToTry expr typeToCompare sub)
                  ( -- if the generateExpressionSuggestion above fails still see if the less greedy version works
                    plugInArgument xs
                  )
          _ ->
            -- no arguments fit
            fail "no arguments fit"
   in plugInArgument candidates

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

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
            (TokenInfo Let _ start end) -> processLet start
            (TokenInfo In _ start end) -> fail "\"in\" needs to be parsed separately"
            _ -> getExpr

      processLambda :: (Int, Int) -> SuggestionBuilder (WithSimplePos Expr, Type, SuggestionBuilderState)
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
                  Nothing -> go otherCandidates -- if lambda type does not match goal try partially applied version
                  Just sub -> do
                    finalExpr <- buildLambdaExpression start (map liftTokenInfoToSimplePos arguments) expr
                    revertTypeEnvironment arguments currentTypeEnv -- we revert the type environment to before we added the arguments of the lambda to the typeEnv.
                    currentState <- getSuggestionBuilderState
                    return (finalExpr, finalType, currentState)
        go candidates

      processLet :: (Int, Int) -> SuggestionBuilder (WithSimplePos Expr, Type, SuggestionBuilderState)
      processLet start =
        let {- processLocalDefinitions :: [[TokenInfo]] -> SuggestionBuilder [(Pattern, [LabelIdentifier], WithSimplePos Expr, Type)]
            processLocalDefinitions list = mapM renameMe list

            renameMe :: [TokenInfo] -> SuggestionBuilder (Pattern, [LabelIdentifier], WithSimplePos Expr, Type)
            renameMe tok = do
              setTokens tok
              freshVar <- getFreshVar
              generateFunctionSuggestion freshVar -}

            generateLetSuggestion :: [([TokenInfo], Maybe [([TokenInfo], Bool)])] -> SuggestionBuilder (WithSimplePos Expr, Type, SuggestionBuilderState)
            generateLetSuggestion localDefConfigs =
              let appendIfJust :: [TokenInfo] -> Maybe [([TokenInfo], Bool)] -> [TokenInfo]
                  appendIfJust base maybeExtra =
                    base ++ case maybeExtra of
                      Just extras -> concatMap fst extras
                      Nothing -> []
               in do
                    -- TODO: make sure that input variables of a local def are overwritten in the type environment and removed for subsequent local defs but restrictions on the rest should remain
                    oldTypeEnv <- getTypeEnvironment

                    let firstName :: [TokenInfo] -> String
                        firstName [] = "ERROR no name found!"
                        firstName (TokenInfo (Name s) _ _ _ : xs) = s
                        firstName (_ : xs) = firstName xs

                        calculateNewTypeEnv :: SuggestionBuilder ([(LabelIdentifier, (Pattern, [LabelIdentifier], WithSimplePos Expr))], [TokenInfo], TypeEnvironment)
                        calculateNewTypeEnv =
                          SuggestionBuilder
                            { runState =
                                \env ->
                                  let currentTypeEnv = getEnvironmentFromState env
                                      newEnv = env {getFreshVarCounterFromState = getFreshVarCounterFromState env + 1}

                                      allEnvs = map (\(tokens, maybeContinuationTokens) -> env {getTokensFromState = removeWhitespace $ appendIfJust tokens maybeContinuationTokens, getEnvironmentFromState = finalEnv}) localDefConfigs
                                      allNames = map (\(tokens, _) -> firstName tokens) localDefConfigs
                                      freshVar = FreshVar $ getFreshVarCounterFromState env
                                      generateFun = runState (generateFunctionSuggestion freshVar)
                                      results = zip allNames $ map (fst . generateFun) allEnvs
                                      states = map (snd . generateFun) allEnvs
                                      (list, finalEnv) =
                                        foldr
                                          ( \(name, maybe) (expressions, env) ->
                                              ( case maybe of
                                                  Justt (a, b, c, typ) -> (name, (a, b, c)) : expressions
                                                  _ -> expressions,
                                                Map.insert
                                                  name
                                                  ( case maybe of
                                                      Justt (_, _, _, typ) -> trace ("#-#-#-#-#-#-#-#-#-" ++ show name ++ show typ) typ
                                                      Error str -> trace str (TypeError str)
                                                  )
                                                  env
                                              )
                                          )
                                          ([], currentTypeEnv)
                                          results
                                   in ( Justt
                                          ( list,
                                            case safeLast states of
                                              Just stat -> getTokensFromState stat
                                              Nothing -> [],
                                            finalEnv
                                          ),
                                        env
                                      )
                            }
                
                    

                    (expressions,remainingTokens , newTypeEnv) <- trace ("group" ++ show (localDefConfigs)) $ calculateNewTypeEnv
                    let localDefs = map snd expressions
                    
                    setTypeEnvironment newTypeEnv
                    --(_, remainingTokensOld) <- localDefsAndRemainingTokens
                    trace ("\nremainingTokens: " ++ show remainingTokens) $ setTokens remainingTokens
                    consumeWhitespace
                    consumeTokenIfExists In
                    consumeWhitespace

                    candidates <- generateExpressionSuggestion goal Nothing []
                    findBestCandidate
                      goal
                      candidates
                      pure
                      ( \expr typ sub -> do
                          applySubstitutionToSuggestionBuilder sub
                          currentState <- getSuggestionBuilderState
                          let (WithSimplePos _ end _) = expr
                          return (WithSimplePos start end $ LetExpression localDefs expr, typ, currentState)
                      )
         in do
              consumeWhitespace
              tokens <- getTokens
              let localDefinitions = splitSections tokens

              let localDefConfigs = trace ("local defs:" ++ show localDefinitions) $ getLocalDefConfigs localDefinitions

              let configs = map generateLetSuggestion localDefConfigs

              firstSuccessful "error! let contrains No definitions" configs

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
              plugInArgument candidates =
                findBestCandidate
                  argTyp
                  candidates
                  pure
                  ( \expr _ sub -> do
                      applySubstitutionToSuggestionBuilder sub
                      let retTypWithSub = applySubstitution sub retTyp
                      currentState <- getSuggestionBuilderState
                      newAccumulator <- addToAccumulator accumulator goal (buildApplication previousExpr expr, retTypWithSub, currentState)
                      res <- generateExpressionSuggestion goal (Just retTypWithSub) newAccumulator
                      -- trace ("fits:" ++ show (map (\(a, b, _) -> (a, b)) res))
                      return res
                  )

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
      revert (TokenInfo (Name name) _ _ _ : restOfTokens) typeEnv = case Map.lookup name oldTypeEnvironment of
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

{- populateLocalDefinitionTokens :: SuggestionBuilder ()
populateLocalDefinitionTokens = do
  localDefs <- getLocalDefinitionTokens
  case localDefs of
    [] -> do
      tokenStream <- getTokens
      setLocalDefinitionTokens $ trace ("tokenSections" ++ show (splitSections tokenStream)) (splitSections tokenStream)
      setTokens []
    _ -> return () -}

-- the bool is to signify if a section contains a let
splitSections :: [TokenInfo] -> [([TokenInfo], Bool)]
splitSections input = map (Data.Bifunctor.first reverse) (splitSections2 [] False input) -- TODO: see if we can get rid of the reverse
  where
    splitSections2 :: [TokenInfo] -> Bool -> [TokenInfo] -> [([TokenInfo], Bool)]
    splitSections2 acc boolAcc [] = [(acc, boolAcc)]
    splitSections2 acc boolAcc (tok@(TokenInfo Newline _ _ _) : tokens) =
      let removeIndent (TokenInfo Indent _ _ _ : tokens) = removeIndent tokens
          removeIndent (TokenInfo Dedent _ _ _ : tokens) = removeIndent tokens
          removeIndent x = x

          checkFunctionPattern (TokenInfo (Name _) _ _ _ : TokenInfo EqualsSign _ _ _ : tokens) = True
          checkFunctionPattern (TokenInfo (Name _) _ _ _ : tokens) = checkFunctionPattern tokens
          checkFunctionPattern _ = False
       in if checkFunctionPattern $ removeIndent tokens
            then (acc, boolAcc) : splitSections2 [] False tokens
            else splitSections2 (tok : acc) boolAcc tokens
    splitSections2 acc boolAcc (tok@(TokenInfo Let _ _ _) : tokens) = splitSections2 (tok : acc) True tokens
    splitSections2 acc boolAcc (tok : tokens) = splitSections2 (tok : acc) boolAcc tokens

-- takes a list of possible local definitions with bools indicating if they contain a let.
-- will return a list ofconfigurations where each configuration is a list of definitions (definition is [TokenInfo]) with a maybe continuation for nested lets
getLocalDefConfigs :: [([TokenInfo], Bool)] -> [[([TokenInfo], Maybe [([TokenInfo], Bool)])]]
getLocalDefConfigs localDefs@((firstLocalDef@(name : rst), containsLet) : rest) =
  let initialIndent = snd $ start_pos name
      restrictions list =
        let restrictions' _ [] = []
            restrictions' acc (True : rest) = (1, acc) : restrictions' 0 rest
            restrictions' acc (False : rest) = restrictions' (acc + 1) rest
         in reverse $ restrictions' 0 $ reverse $ map snd localDefs

      possibleConfigs = mapM range (restrictions localDefs) -- range turns e.g (1,4) into [1,2,3,4] and mapM is equivalent to sequence . map
      getIndent :: [TokenInfo] -> Int
      getIndent (name : _) = snd $ start_pos name
      getIndent [] = 0

      indentDifferences = map (Data.Bifunctor.first ((\x -> abs (x - initialIndent)) . getIndent)) localDefs

      keepMask :: [(a, Bool)] -> [Int] -> [Bool]
      keepMask list ints =
        let keepMask' _ [] _ = []
            keepMask' _ ((x, True) : xs) [] = error "ERROR for developer: found a let without config THIS SHOULD NEVER HAPPEN!"
            keepMask' 0 ((x, False) : xs) ints = True : keepMask' 0 xs ints
            keepMask' 0 ((x, True) : xs) (currentAmount : ints) = True : keepMask' currentAmount xs ints
            keepMask' toIgnore ((x, False) : xs) ints = False : keepMask' (toIgnore - 1) xs ints
            keepMask' toIgnore ((x, True) : xs) (currentAmount : ints) = False : keepMask' currentAmount xs ints
         in keepMask' 0 list ints

      groupByFlag :: [(a, Bool)] -> [[(a, Bool)]]
      groupByFlag =
        let groupByFlag' acc [] = [acc]
            groupByFlag' acc ((a, True) : xs) = acc : groupByFlag' [(a, True)] xs
            groupByFlag' acc ((a, False) : xs) = groupByFlag' ((a, False) : acc) xs
         in map reverse . filter (not . null) . groupByFlag' []

      configFilter :: (Show a) => ([(a, Bool)] -> b) -> [(a, Bool)] -> [Int] -> [(a, Maybe b)]
      configFilter fun defs ints =
        let groups = groupByFlag $ zip (map fst defs) (keepMask defs ints)
         in map
              ( \group -> case group of
                  [x] -> (fst x, Nothing)
                  (x : xs) -> (fst x, Just (fun xs))
              )
              groups

      getWeight ints = sum $ map fst $ configFilter (\_ -> ()) indentDifferences ints

      sortedConfigs = sortOn ((*) (-1) . getWeight) possibleConfigs
   in trace ("debugging" ++ show (localDefs)) $ map (configFilter id localDefs) sortedConfigs