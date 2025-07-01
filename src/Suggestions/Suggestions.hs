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
import Control.Monad (foldM_)
import Data.Array (Ix (range))
import qualified Data.Bifunctor
import Data.Foldable
import Data.Function (on)
import Data.List (groupBy, sortBy, sortOn)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Text.Internal.Fusion.Size as Map
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
import Suggestions.TokenDifference (Action (..), TargetTokens, generateActions, recreateOriginalWithDifferencesShow, sectionToSuggestion)
import qualified System.IO.Error as Map
import Text.Megaparsec (MonadParsec (token), ParseErrorBundle, Stream (Token), between, errorBundlePretty)
import TypeInference.TypeInference
  ( Errorable (..),
    FailMessage (..),
    MaybeError (..),
    Substitution,
    applySubstitution,
    applySubstitutionToTypeEnvironment,
    freshVar,
    generateFreshVars,
    mostGeneralUnifier,
  )
import Text.Megaparsec.Byte (string)

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
generateSuggestion :: FreshVariableCounter -> Map.Map String Type -> [TokenInfo] -> MaybeError ([TargetTokens], String, String, Type, Int)
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
        (typeGoal, updatedTypeEnv) <- case Map.lookup functionName typeEnv of
          Just x -> return (x, typeEnv) --dropTypeArguments arguments typeEnv x
          Nothing -> Justt (FreshVar $ state + length argumentTypeVars, Map.union argumentTypeVars typeEnv)
        let builder =
              do
                {- trace ("typgoal of function:" ++ show (Map.lookup functionName typeEnv)) $ -}
                setTypeEnvironment $ Map.map (Map.singleton (Scope 0 0) . Const) updatedTypeEnv
                generateFunctionSuggestion typeGoal
        (suggestion, builderState) <- runSuggestionBuilder (state + length argumentTypeVars + 1) builder tokens

        let (functionName, arguments2, body, finalType) = suggestion
        let section = FunctionDefinition functionName arguments body
        let (expectedTokens, _) = sectionToSuggestion section
        let tokenList = map (\(TokenInfo tok _ _ _) -> tok) tokens
        let actionTokens = generateActions expectedTokens tokenList
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

type UUID = Type

data TypePromise
  = Const Type
  | Function Depth (SuggestionBuilder ([TokenInfo], (Pattern, [LabelIdentifier], WithSimplePos Expr, Type))) UUID
  | Result Type Pattern [LabelIdentifier] (WithSimplePos Expr) [TokenInfo] -- TODO: see what we actually need and make this smaller

instance Eq TypePromise where
  (Const t1) == (Const t2) = t1 == t2
  (Function d1 _ u1) == (Function d2 _ u2) = d1 == d2 && u1 == u2
  (Result t1 p1 l1 e1 toks1) == (Result t2 p2 l2 e2 toks2) =
    t1 == t2 && p1 == p2 && l1 == l2 && e1 == e2 && toks1 == toks2
  _ == _ = False

instance Show TypePromise where
  show (Const typ) = "Const: " ++ show typ
  show (Function depth _ _) = "<function at depth " ++ show depth ++ ">"
  show (Result typ _ _ _ _) = "Result: " ++ show typ

--
data Scope = Scope Depth Branch
  deriving (Show, Ord, Eq)

type SuggestionBuilderTypeEnvironment = Map.Map String (Map.Map Scope TypePromise)

type Depth = Int

type Branch = Int

data SuggestionBuilderState = State
  { getTokensFromState :: [TokenInfo],
    getEnvironmentFromState :: SuggestionBuilderTypeEnvironment,
    getFreshVarCounterFromState :: Int,
    getBranchCounterFromState :: Int, -- for performance
    getScopeFromState :: Scope -- depth followed by current branch
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
              getBranchCounterFromState = 0,
              -- getLocalDefinitionTokensFromState = []
              getScopeFromState = Scope 0 0 -- depth followed by current branch
            }
   in do
        a <- ma
        return (a, state)

getSuggestionBuilderState :: SuggestionBuilder SuggestionBuilderState
getSuggestionBuilderState =
  SuggestionBuilder {runState = \state -> (Justt state, state)}

getScope :: SuggestionBuilder Scope
getScope =
  getScopeFromState <$> getSuggestionBuilderState

increaseScopeDepth :: SuggestionBuilder ()
increaseScopeDepth = do
  (Scope depth branch) <- getScope
  setScope (Scope (depth + 1) branch)

setScope :: Scope -> SuggestionBuilder ()
setScope scope = do
  state <- getSuggestionBuilderState
  setSuggestionBuilderState state {getScopeFromState = scope}

setSuggestionBuilderState :: SuggestionBuilderState -> SuggestionBuilder ()
setSuggestionBuilderState state = SuggestionBuilder {runState = \_ -> (Justt (), state)}

getTypeEnvironment :: SuggestionBuilder SuggestionBuilderTypeEnvironment
getTypeEnvironment = getEnvironmentFromState <$> getSuggestionBuilderState

setTypeEnvironment :: SuggestionBuilderTypeEnvironment -> SuggestionBuilder ()
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

applySubstitutionToTypePromise :: Substitution -> TypePromise -> TypePromise
applySubstitutionToTypePromise sub (Const typ) = Const $ applySubstitution sub typ
applySubstitutionToTypePromise sub fun@(Function {}) = fun -- TODO: properly implement this
applySubstitutionToTypePromise sub (Result typ x1 x2 x3 x4) = Result (applySubstitution sub typ) x1 x2 x3 x4 -- TODO: properly implement this

applySubstitutionToSuggestionBuilderTypeEnvironment :: Substitution -> SuggestionBuilderTypeEnvironment -> SuggestionBuilderTypeEnvironment
applySubstitutionToSuggestionBuilderTypeEnvironment substitution typeEnv = Map.map (Map.map (applySubstitutionToTypePromise substitution)) typeEnv

applySubstitutionToSuggestionBuilder :: Substitution -> SuggestionBuilder ()
applySubstitutionToSuggestionBuilder sub =
  do
    currentEnv <- getTypeEnvironment
    let newEnv = applySubstitutionToSuggestionBuilderTypeEnvironment sub currentEnv
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

-- | drops any tokens until it finds a name or number or anything in the targets list (1st argument) 
--   should probably contain [TrueToken,FalseToken] in most cases
removeAnyUntil :: [Lexer.Tokens.Token]-> [TokenInfo] -> [TokenInfo]
removeAnyUntil targets [] = []
removeAnyUntil targets input@(TokenInfo (Name _)_ _ _ : xs) = input --always stop on a name
removeAnyUntil targets input@(TokenInfo (Number _)_ _ _ : xs) = input --always stop on a number
removeAnyUntil targets input@(x: xs) = if token_type x `elem` targets then
                      input --stop
                    else
                      removeAnyUntil targets xs

-- | drops any tokens until it finds a name or number or anything in the targets list (1st argument) 
--   should probably contain [TrueToken,FalseToken] in most cases
consumeAnyUntil :: [Lexer.Tokens.Token]-> SuggestionBuilder ()
consumeAnyUntil targets = do
  tokens <- getTokens
  setTokens $ removeAnyUntil targets tokens

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

getTypesFromState :: String -> SuggestionBuilder (Map.Map Scope TypePromise)
getTypesFromState key = do
  env <- getTypeEnvironment
  liftError $ Map.lookup key env

safeFirst :: [a] -> Maybe a
safeFirst [] = Nothing
safeFirst (x : _) = Just x

findType :: Scope -> Map.Map Scope TypePromise -> Maybe (Scope, TypePromise)
findType scope map = safeFirst $ getTypesInScope scope map

-- | gets all the types that are in scope (depth <= and on the same branch).
--
-- gives the largest depth first.
getTypesInScope :: Scope -> Map.Map Scope TypePromise -> [(Scope, TypePromise)]
getTypesInScope (Scope depthTarget currentBranch) map = filter (\(Scope depth branch, _) -> depth <= depthTarget && branch == currentBranch) $ Map.toDescList map

getTypeFromState :: String -> SuggestionBuilder (Type, Maybe TypePromise)
getTypeFromState identifier = do
  current <- getTypesFromState identifier
  scope <- getScope
  (foundScope, promise) <- {- trace ("getting " ++ show identifier ++" at scope: " ++ show scope )$ -} liftError (findType scope current)
  case promise of
    Const typ -> return (typ, Nothing)
    Function depth func _ -> do
      tokensToRevertTo <- getTokens
      depthToRevertTo <- {- trace ("branching at:" ++ show depth) $ -} branchAt depth
      -- TODO: remove the function here and replace with some error/(fresh var and unify with it later to check that we are stable???)
      (remainingTokens, (name, label, expr, typ)) <- func
      typeEnvBefore <- getTypeEnvironment
      {- trace ("-----------env before unbranching: " ++ show typeEnvBefore) $ -} 
      unBranch depthToRevertTo
      let newPromise = Result typ name label expr remainingTokens
      typeEnv <- getTypeEnvironment
      {-  trace ("-----------env after unbranching: " ++ show typeEnv) $  -}
      setTypeEnvironment $ Map.update (Just . Map.map (\x -> if x == promise then newPromise else x)) identifier typeEnv
      setTokens tokensToRevertTo -- make sure we do set the remaining tokens to be eaten back to where we were.
      --TODO: unbranch at some point????
      return (typ, Just newPromise)
    Result typ _ _ _ _ -> return (typ, Just promise)

branchAt :: Depth -> SuggestionBuilder Depth
branchAt depth = do
  (Scope oldDepth branch) <- getScope
  startingEnv <- getTypeEnvironment
  let newBranch = branch + 1
  let lookupScope = Scope depth branch

  let newEnv :: SuggestionBuilderTypeEnvironment
      newEnv =
        Map.map
          ( \item -> case getTypesInScope lookupScope item of
              [] -> item -- if not in scope don't add it
              list ->
                foldr
                  ( \(Scope depth _, typePromise) currentMap ->
                      Map.insert (Scope depth newBranch) typePromise currentMap
                  )
                  item
                  list -- add every in scope item to the new typeEnv with the new Branch
          )
          startingEnv

  setTypeEnvironment newEnv
  setScope $ Scope depth newBranch
  return oldDepth

-- | backtrack the latest branch
unBranch :: Depth ->  SuggestionBuilder ()
unBranch depthToRevertTo = do
  (Scope _ currentBranch) <- getScope
  startingEnv <- getTypeEnvironment
  let newEnv :: SuggestionBuilderTypeEnvironment
      newEnv =
        Map.mapMaybe
          ( \item ->
              let newItem =
                    Map.mapMaybeWithKey
                      (\(Scope _ branch) i -> if branch == currentBranch then Nothing else Just i) -- remove all entries at the current branch
                      item
               in if Map.null newItem -- if the new Scope TypePromise map is empty also remove it from the big String-map
                    then Nothing
                    else Just newItem
          )
          startingEnv
  setTypeEnvironment newEnv
  setScope $ Scope depthToRevertTo (currentBranch - 1)

-- will insert a value into the type env at the current scope, if the current scope already exists it will replace the value
insertIntoTypeEnvironment :: String -> Type -> SuggestionBuilder ()
insertIntoTypeEnvironment identifier typ = {- trace "doing insertIntoTypeEnvironment" $ -} do
  env <- {- trace "scope" -} getTypeEnvironment
  scope <-{-  trace "scope" -} getScope
  setTypeEnvironment $ Map.insertWith Map.union identifier (Map.singleton scope (Const typ)) env

dropTypeArguments2 :: [LabelIdentifier] -> Type -> SuggestionBuilder (Type, SuggestionBuilderTypeEnvironment)
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
      insertIntoTypeEnvironment identifier a
      dropTypeArguments2 xs b
    (identifier : xs, _) -> fail $ "Cannot take an argument from the type " ++ show typ ++ " to assign to " ++ show identifier ++ " which has type " ++ show typ ++ "!"

generateFunctionSuggestion :: Type -> SuggestionBuilder (Pattern, [LabelIdentifier], WithSimplePos Expr, Type)
generateFunctionSuggestion goal =
  let getNextName = do
        tokens <- getTokens
        tok <- getToken (fail $ "Not Enough tokens to satisfy goal:\n" ++ show goal ++ "\n!")
        case tok of
          (TokenInfo (Name name) _ start end) -> return name
          _ -> fail $ "name is not next, found: " ++ show tok ++ " instead \n\tfor: " ++ show tokens

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
        let go argsIn = case argsIn of
              [] -> return exprTyp
              identifier : xs ->
                do
                  (argType, _) <- getTypeFromState identifier
                  resultType <- go xs
                  return $ TypeArrow argType resultType
         in do
              go args
   in do
        functionName <- getNextName
        arguments <- many getNextName
        --trace ("function name in let:" ++ functionName)
        consumeWhitespace
        consumeTokenIfExists EqualsSign

        freshVars <- getFreshVars (length arguments)
        let pairs = zip arguments freshVars
        startingTypeEnv <- --trace ("fresh vars:" ++ show freshVars) 
          getTypeEnvironment

        foldM_ (\acc (argumentName, argumentType) -> insertIntoTypeEnvironment argumentName argumentType) () pairs

        (expressionGoal, updatedTypeEnv) <- {- trace ("goal:" ++ show goal) $ -} dropTypeArguments2 arguments goal
        --trace ("expressionGoal:" ++ show expressionGoal) $ 
        setTypeEnvironment updatedTypeEnv

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

-- | goes trough the list and finds the first candidate that fits the goal. It executes the function that will get the expression, its type and the substitution needed to plug it in. 
findBestCandidate :: Type -> [Candidate] -> (Type -> SuggestionBuilder Type) -> (WithSimplePos Expr -> Type -> Substitution -> SuggestionBuilder b) -> SuggestionBuilder b
findBestCandidate goal candidates typeTransform suggestionBuilderToTry =
  --trace ("finding best candidates for " ++ show candidates) $
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
              fail "no arguments fit 345346"
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
                (typ, _) <- getTypeFromState name <?> "could not find " ++ name ++ " in type environment"
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
        originalScope <- getScope
        increaseScopeDepth
        unconsumedArguments <- addArgumentsToTypeEnvironment goalArguments arguments

        let expressionGoal = buildTypeFromArguments ({- trace "unconsumedArguments#############" -} unconsumedArguments) ({- trace "goalReturnType######################" -} goalReturnType)
        candidates <- generateExpressionSuggestion ({- trace "expressionGoal" -} expressionGoal) Nothing []
        let go candidateList = case candidateList of
              [] -> fail $ "could not generate an expression for this lambda stating at " ++ show start
              (expr, typ, env) : otherCandidates -> do
                setSuggestionBuilderState env -- if we partially apply we need to revert the state.
                newTypeEnv <- getTypeEnvironment

                argumentScope <- getScope

                typeArguments <-{-  trace ("Scope: " ++ show argumentScope) $ -} mapM (\(TokenInfo (Name name) _ _ _) -> fst <$> getTypeFromState name) arguments
                let finalType = buildTypeFromArguments typeArguments typ
                case mostGeneralUnifier finalType goal of
                  Nothing -> go otherCandidates -- if lambda type does not match goal try partially applied version
                  Just sub -> do
                    finalExpr <- buildLambdaExpression start (map liftTokenInfoToSimplePos arguments) expr
                    foldM_ (\_ (TokenInfo (Name name) _ _ _) -> removeTypeFromEnvironment argumentScope name) () arguments -- we revert the type environment to before we added the arguments of the lambda to the typeEnv.
                    setScope originalScope -- we are done with the lambda so revert to the original scope
                    currentState <- getSuggestionBuilderState
                    return (finalExpr, finalType, currentState)
        go ({- trace "candidates" -} candidates)

      processLet :: (Int, Int) -> SuggestionBuilder (WithSimplePos Expr, Type, SuggestionBuilderState)
      processLet start =
        let
            -- | takes a configuration and uses it to build a suggestion
            generateLetSuggestion :: [([TokenInfo], Maybe [([TokenInfo], Bool)])] -> SuggestionBuilder (WithSimplePos Expr, Type, SuggestionBuilderState)
            generateLetSuggestion localDefConfigs =
              let appendIfJust :: [TokenInfo] -> Maybe [([TokenInfo], Bool)] -> [TokenInfo]
                  appendIfJust base maybeExtra =
                    base ++ case maybeExtra of
                      Just extras -> concatMap fst extras
                      Nothing -> []

                  firstName :: [TokenInfo] -> String
                  firstName [] = "ERROR no name found!"
                  firstName (TokenInfo (Name s) _ _ _ : xs) = s
                  firstName (_ : xs) = firstName xs
               in do
                    oldTypeEnv <- getTypeEnvironment
                    originalScope@(Scope depth branch) <- getScope
                    let newDepth = depth + 1
                    let newScope = Scope newDepth branch
                    setScope newScope

                    uuids <- getFreshVars (length localDefConfigs)

                    let typePromiseEntries :: [(String, TypePromise)]
                        typePromiseEntries =
                          zipWith (curry ( \((tokens, cont),uuid) ->
                                ( firstName tokens,
                                  Function
                                    newDepth
                                    ( do
                                        setTokens $ appendIfJust tokens cont
                                        freshVar <- getFreshVar
                                        consumeWhitespace
                                        res <- generateFunctionSuggestion freshVar
                                        leftOverTokes <- getTokens
                                        return (leftOverTokes, res)
                                    )
                                    uuid
                                )
                            )) localDefConfigs uuids

                    let newTypeEnvironment :: SuggestionBuilderTypeEnvironment
                        newTypeEnvironment = foldr (\(name, promise) env -> Map.insertWith (\_newSingletonMap existingMap -> Map.insert newScope promise existingMap) name (Map.singleton newScope promise) env) oldTypeEnv typePromiseEntries

                    {- trace ("typePromiseEntries:" ++ show typePromiseEntries) $ -} 
                    setTypeEnvironment newTypeEnvironment

                    results <- map snd <$> mapM (\(name, _) -> {- trace ("getting: " ++ name ++ " from: "++ show newTypeEnvironment) $ -} getTypeFromState name) typePromiseEntries

                    let extractResults :: [Maybe TypePromise] -> ([(Pattern, [LabelIdentifier], WithSimplePos Expr)], [TokenInfo])
                        extractResults list =
                          foldr
                            ( \maybePromise acc@(localDef,remainingTokens)  -> case (maybePromise,remainingTokens) of
                                (Just (Result _typ name labels expr restOfTokens),[]) -> ( (name,labels,expr): localDef,restOfTokens) -- if the remainingTokens is still empty we are in the first loop and can then use the first remainingTokens
                                (Just (Result _typ name labels expr restOfTokens),_) -> ( (name,labels,expr): localDef,remainingTokens) -- otherwise we have already seen the remainingTokens so we don't need to consider the others
                                _ -> trace ("error! one of the local defs wasn't a result instead we got: " ++ show maybePromise) acc
                            )
                            ([], [])
                            list

                    let (localDefinitions,remainingTokens) = extractResults results

                    {- trace ("##########remainingTokens:" ++ show remainingTokens) $  -}
                    setTokens remainingTokens


                    consumeAnyUntil [TrueToken,FalseToken,In] 

                    consumeTokenIfExists In

                    candidates <- generateExpressionSuggestion goal Nothing []

                    findBestCandidate
                                goal
                                candidates
                                pure
                                ( \expr typ sub -> do
                                    applySubstitutionToSuggestionBuilder sub
                                    currentState <- getSuggestionBuilderState
                                    let (WithSimplePos _ end _) = expr
                                    setScope originalScope -- revert the scope after the let
                                    return (WithSimplePos start end $ LetExpression localDefinitions expr, typ, currentState)
                                )


         in
            do
              consumeWhitespace
              tokens <- getTokens
              let localDefinitions = splitSections tokens

              let localDefConfigs = {- trace ("local defs:" ++ show localDefinitions) $  -}getLocalDefConfigs localDefinitions

              let configs = map generateLetSuggestion localDefConfigs

              firstSuccessful "error! let contains No definitions" configs

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
addArgumentsToTypeEnvironment types [] = {- trace "done with addArgumentsToTypeEnvironment" $  -}return types
addArgumentsToTypeEnvironment (typ : restOfTypes) (x : xs) = do
  -- if there is a type given use it
  addLabelToTypeEnvironment typ x
  addArgumentsToTypeEnvironment restOfTypes xs
addArgumentsToTypeEnvironment [] (x : xs) = do
  -- if there is no type given use a fresh variable
  {- trace "adding fresh var" -} 
  addLabelToTypeEnvironmentAsFreshVar ({- trace ("x: " ++ show x) -} x)
  addArgumentsToTypeEnvironment [] xs

addLabelToTypeEnvironment :: Type -> TokenInfo -> SuggestionBuilder ()
addLabelToTypeEnvironment typ (TokenInfo (Name name) _ start end) = do
  insertIntoTypeEnvironment name typ
addLabelToTypeEnvironment _ _ = fail "internal ERROR addLabelToTypeEnvironmentAsFreshVar was not given a Name token!"

addLabelToTypeEnvironmentAsFreshVar :: TokenInfo -> SuggestionBuilder ()
addLabelToTypeEnvironmentAsFreshVar (TokenInfo (Name name) _ start end) = {- trace "addLabelToTypeEnvironmentAsFreshVar" $  -}do
  typeEnv <- getTypeEnvironment
  freshVar <- {- trace "getFreshVar" -} getFreshVar
  {- trace ("typeEnv: " ++ show typeEnv) $  -}
  insertIntoTypeEnvironment name freshVar
  typeEnv2 <- getTypeEnvironment
  return ({- trace ("typeEnv2: " ++ show typeEnv2) -} ())
addLabelToTypeEnvironmentAsFreshVar _ = fail "internal ERROR addLabelToTypeEnvironmentAsFreshVar was not given a Name token!"

revertTypeEnvironment :: [TokenInfo] -> SuggestionBuilderTypeEnvironment -> SuggestionBuilder ()
revertTypeEnvironment tokens oldTypeEnvironment = do
  newTypeEnvironment <- getTypeEnvironment
  let revert [] typeEnv = typeEnv
      revert (TokenInfo (Name name) _ _ _ : restOfTokens) typeEnv = case Map.lookup name oldTypeEnvironment of
        Nothing -> revert restOfTokens $ Map.delete name typeEnv
        Just typ -> revert restOfTokens $ Map.insert name typ typeEnv
  setTypeEnvironment $ revert tokens newTypeEnvironment

removeTypeFromEnvironment :: Scope -> String -> SuggestionBuilder ()
removeTypeFromEnvironment scope identifier = do
  env <- getTypeEnvironment
  setTypeEnvironment $
    Map.update
      ( \map ->
          let newMap = Map.delete scope map
           in ( if null newMap
                  then Nothing
                  else Just newMap
              )
      )
      identifier
      env

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
-- will return a list of configurations where each configuration is a list of definitions (definition is [TokenInfo]) with a maybe continuation for nested lets
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
   in {- trace ("debugging" ++ show (localDefs)) $ -} map (configFilter id localDefs) sortedConfigs