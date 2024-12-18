{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use const" #-}
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
import Parser.ParserBase
import Parser.Types
import Suggestions.TokenDifference (Action (..), ExtendedTokens, generateActions, recreateOriginalWithDifferencesShow, testfunc)
import Text.Megaparsec (ParseErrorBundle, between, errorBundlePretty)
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

generateSuggestion :: Int -> Map.Map String Type -> [TokenInfo] -> MaybeError ([ExtendedTokens], String, String, Type, Int)
generateSuggestion state typeEnv tokens =
  let functionName = getSectionName tokens
      getArguments :: [TokenInfo] -> [WithSimplePos LabelIdentifier]
      getArguments list = case list of
        (TokenInfo EqualsSign _ _ _ : _) -> []
        (TokenInfo (Name name) _ start end : xs) -> WithSimplePos start end name : getArguments xs
        (TokenInfo tok _ start end : xs) -> WithSimplePos start end ("weird token " ++ show tok) : getArguments xs
        _ -> []

      arguments :: [WithSimplePos LabelIdentifier]
      arguments = getArguments (tail tokens)

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
                generateExpressionSuggestion typeGoal
        ((expr, typ), builderState) <- runSugesstionBuilder (state + length argumentTypeVars + 1) builder expressionTokens
        let section = FunctionDefinition functionName arguments expr
        let (expectedTokens, _) = testfunc section
        let tokenList = map (\(TokenInfo tok _ _ _) -> tok) tokens
        let actionTokens = generateActions expectedTokens tokenList
        finalType <- buildReturnType (getEnvironmentFromState builderState) arguments typ
        trace (show (getEnvironmentFromState builderState)) $
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
    setTypeEnvironment $ Map.insert ("added " ++ show sub) (TypeCon TypeBool) $ applySubstitutionToTypeEnvironment sub currentEnv

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
          trace str $
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

generateExpressionSuggestion :: Type -> SuggestionBuilder (WithSimplePos Expr, Type)
generateExpressionSuggestion goal =
  let -- getArguments returns the list of arguments that can be applied to the function, consuming the tokens in the process.
      -- it also gives the type of the expression up to that point.
      -- See Note [suggestions/getArguments]
      getArguments :: Type -> SuggestionBuilder [(WithSimplePos Expr, Type, SuggestionBuilderState)]
      getArguments remainingType =
        let takeMoreArgs = case remainingType of
              TypeArrow argTyp retTyp -> do
                (argExpr, _) <- generateExpressionSuggestion argTyp
                stateAfterThisArg <- getSuggestionBuilderState
                arguments <- getArguments retTyp
                return $ (argExpr, retTyp, stateAfterThisArg) : arguments
              _ -> return [] -- not a function.
         in try takeMoreArgs (return [])
   in do
        tok <- getToken (fail $ "Not Enough tokens to satisfy goal:\n" ++ show goal ++ "\n!")
        case tok of
          -- we have a function or constant
          (TokenInfo (Name name) _ start end) -> do
            typeEnv <- getTypeEnvironment
            typ <- liftError (Map.lookup name typeEnv) <?> "could not find " ++ name ++ " in type environment"
            argsAndTypes <- getArguments typ -- get the arguments to the function
            let processArguments list =
                  case list of
                    -- there are no arguments
                    [] -> case mostGeneralUnifier typ goal of
                      Nothing -> fail $ "could not unify:\n\t" ++ show typ ++ "\nwith the goal:\n\t" ++ show goal ++ "\nas result of the function \"" ++ name ++ "\""
                      Just sub -> do
                        applySubstitutionToSuggestionBuilder sub
                        return (WithSimplePos start end (Label name), typ) -- todo: should the typ be goal instead?
                        -- there are arguments for the function
                    _ -> do
                      let args = reverse $ map (\(x, _, _) -> x) list
                      let retTyp = (\(_, y, _) -> y) $ head list
                      let argumentExpression = foldl' (\item expr -> WithSimplePos start end (Application item expr)) (WithSimplePos start end (Label name)) args
                      case mostGeneralUnifier retTyp goal of
                        Nothing -> processArguments $ tail list
                        Just sub -> do
                          applySubstitutionToSuggestionBuilder sub
                          return (argumentExpression, retTyp)

                argumentAtDifferentPlace functionType =
                  case typeToArguments functionType of
                    (typeArguments, returnType) ->
                      do
                        freshVar <- getFreshVar
                        (expr, foundArgumentType) <- generateExpressionSuggestion freshVar

                        case firstMatchToFront foundArgumentType typeArguments of
                          Nothing -> fail $ show foundArgumentType ++ " matches none of the arguments of " ++ show goal
                          Just (indexFromBack, fittingArgumentsSequence) -> do
                            let cycledType = buildTypeFromArguments (drop 1 fittingArgumentsSequence) returnType
                            newArgsAndTypes <- getArguments cycledType

                            (newExpr, typ) <-
                              {- trace
                                ( "\n\nfound: "
                                    ++ show foundArgumentType
                                    ++ " instead of expected: "
                                    ++ show currentTarget
                                    ++ "\nwe did find: "
                                    ++ show cycledType
                                    ++ " where we swapped on index "
                                    ++ show indexFromBack
                                    ++ "\n\n"
                                ) -}

                              try
                                ( do
                                    res@(_, finalType) <- processArguments $ reverse newArgsAndTypes
                                    case finalType of -- if the result would be a function double check if swapping arguments is needed
                                      TypeArrow _ _ -> try (argumentAtDifferentPlace cycledType) (return res)
                                      _ -> return res
                                )
                                (argumentAtDifferentPlace cycledType)

                            return ({- trace ("\n newExpr" ++ show (expr, indexFromBack, newExpr)) -} insertIntoExpressionAtIndex indexFromBack expr newExpr, typ)
                    -- fail $ "try swapping arguments" ++ show (buildTypeFromArguments cycledArguments returnType)
                    _ -> fail "not enough arguments to swap"
             in try
                  ( do
                      res@(_, finalType) <- processArguments $ reverse argsAndTypes
                      case finalType of -- if the result would be a function double check if swapping arguments is needed
                        TypeArrow _ _ -> try (argumentAtDifferentPlace typ) (return res)
                        _ -> return res
                  )
                  (argumentAtDifferentPlace typ)
          (TokenInfo (Number num) _ start end) ->
            case mostGeneralUnifier (TypeCon TypeInt) goal of
              Nothing -> fail $ "Int does not match " ++ show goal
              Just sub -> do
                applySubstitutionToSuggestionBuilder sub
                return (WithSimplePos start end (Int num), TypeCon TypeInt)
          (TokenInfo TrueToken _ start end) ->
            case mostGeneralUnifier (TypeCon TypeBool) goal of
              Nothing -> fail $ "Bool does not match " ++ show goal
              Just sub -> do
                applySubstitutionToSuggestionBuilder sub
                return (WithSimplePos start end (Bool True), TypeCon TypeBool)
          (TokenInfo FalseToken _ start end) ->
            case mostGeneralUnifier (TypeCon TypeBool) goal of
              Nothing -> fail $ "Bool does not match " ++ show goal
              Just sub -> do
                applySubstitutionToSuggestionBuilder sub
                return (WithSimplePos start end (Bool False), TypeCon TypeBool)
          _ -> generateExpressionSuggestion goal

firstMatchToFront :: Type -> [Type] -> Maybe (Int, [Type])
firstMatchToFront foundType goals =
  let go (x : xs) index =
        case mostGeneralUnifier foundType x of
          Just _ -> Just (index, x, xs)
          Nothing -> do
            (a, b, c) <- go xs (index - 1)
            return (a, b, x : c)
      go [] _ = Nothing
   in do
        (index, match, rest) <- go goals (length goals - 1)
        return (index, match : rest)

insertIntoExpressionAtIndex :: Int -> WithSimplePos Expr -> WithSimplePos Expr -> WithSimplePos Expr
insertIntoExpressionAtIndex index expr@(WithSimplePos start end _) originalExpr =
  case (index, originalExpr) of
    (0, WithSimplePos startRest endRest _) -> WithSimplePos start endRest (Application originalExpr expr) -- TODO: figue out propper start and end positions
    (_, WithSimplePos startRest endRest (Application exp1 expr2)) ->
      WithSimplePos
        startRest
        endRest
        (Application (insertIntoExpressionAtIndex (index - 1) expr exp1) expr2)
    _ -> originalExpr

-- TODO: add an error case here for when the originalexpr is not an application.Action
