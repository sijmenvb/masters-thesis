{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
module Suggestions.Suggestions where

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Void
import Lexer.Tokens (Token (..), TokenInfo (TokenInfo, token_type))
import Parser.ParserBase
import Parser.Types
import Suggestions.TokenDifference (testfunc, ExtendedTokens, generateActions, Action)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import TypeInference.TypeInference

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

-- generateSuggestion :: InferenceState -> TypeEnvironment -> [TokenInfo] -> MaybeError (Section, Type)
--generateSuggestion :: Int-> Map.Map String Type-> [TokenInfo]-> MaybeError ([Suggestions.TokenDifference.Action Token], Type)
generateSuggestion state typeEnv tokens =
  let functionName = getSectionName tokens
      getArguments :: [TokenInfo] -> [WithSimplePos LabelIdentifier]
      getArguments list = case list of
        (TokenInfo EqualsSign _ _ _ : _) -> []
        (TokenInfo (Name name) _ start end : xs) -> WithSimplePos start end name : getArguments xs
        (TokenInfo tok _ start end : xs) -> WithSimplePos start end ("weird token " ++ show tok) : getArguments xs
        _ -> []
      arguments = getArguments (drop 1 tokens)
      argumentTypeVars :: TypeEnvironment
      argumentTypeVars = Map.fromList $ indexedMap (\index (WithSimplePos _ _ name) -> (name, FreshVar $ state + index)) arguments
      expressionTokens = (drop 1 . dropWhile (\tokenInfo -> token_type tokenInfo /= EqualsSign)) tokens

      typeGoal :: Type
      typeGoal = case Map.lookup functionName typeEnv of
        Just x -> x
        Nothing -> FreshVar $ state + length argumentTypeVars
   in do
        (expr, typ) <- runSugesstionBuilder (generateExpressionSuggestion typeEnv typeGoal) expressionTokens
        let section = FunctionDefinition functionName arguments expr
        let (expectedTokens ,_) = testfunc section
        let tokenList = map (\(TokenInfo tok _ _ _) -> tok) tokens
        return ( expectedTokens,generateActions expectedTokens tokenList, typ)

-- <?> "could not generate a suggestion for " ++ show name ++ " " ++ show arguments ++ " " ++ show expressionTokens

newtype SuggestionBuilder a = RunState ([TokenInfo] -> (MaybeError a, [TokenInfo]))

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

runSugesstionBuilder :: SuggestionBuilder a -> [TokenInfo] -> MaybeError a
runSugesstionBuilder (RunState run) tokens = fst (run tokens)

getTokens :: SuggestionBuilder [TokenInfo]
getTokens =
  RunState (\state -> (Justt state, state))

popToken :: SuggestionBuilder TokenInfo
popToken =
  RunState
    ( \state -> case state of
        [] -> (Error "no items to pop", state)
        (x : xs) -> (Justt x, xs)
    )

hasTokens :: SuggestionBuilder Bool
hasTokens =
  RunState
    ( \state -> case state of
        [] -> (Justt False, state)
        _ -> (Justt True, state)
    )

try :: SuggestionBuilder a -> SuggestionBuilder a -> SuggestionBuilder a
try (RunState run1) (RunState run2) =
  RunState
    ( \state -> case run1 state of
        res@(Justt _, _) -> res
        (Error _, _) -> run2 state
    )

generateExpressionSuggestion :: TypeEnvironment -> Type -> SuggestionBuilder (WithSimplePos Expr, Type)
generateExpressionSuggestion typeEnv goal =
  let getArguments remainingType =
        case remainingType of
          TypeArrow argTyp retTyp -> do
            (argExpr, typ) <- generateExpressionSuggestion typeEnv argTyp
            case retTyp of
              TypeArrow argTyp2 retTyp2 ->
                case (not . null $ mostGeneralUnifier retTyp goal, not . null $ mostGeneralUnifier retTyp2 goal) of
                  (True, True) ->
                    try
                      ( do
                          arguments <- getArguments retTyp
                          return $ (argExpr, retTyp) : arguments
                      )
                      (return [(argExpr, retTyp)])
                  _ -> do
                    arguments <- getArguments retTyp
                    return $ (argExpr, typ) : arguments
              _ -> return [(argExpr, typ)]
          _ -> fail "too many arguments"
   in do
        hasTok <- hasTokens
        if hasTok
          then do
            tok <- popToken
            case tok of
              (TokenInfo (Name name) _ start end) -> do
                typ <- liftError (Map.lookup name typeEnv) <?> "could not find " ++ name ++ " in type environment"
                hasTok2 <- hasTokens
                if not hasTok2
                  then
                    if null (mostGeneralUnifier typ goal)
                      then fail "could not build up type"
                      else return (WithSimplePos start end (Label name), typ)
                  else do
                    argsAndTypes <- getArguments typ
                    let args = map fst argsAndTypes
                    let retTyp = snd $ last argsAndTypes
                    let argumentExpression = foldl (\item expr -> WithSimplePos start end (Application item expr)) (WithSimplePos start end (Label name)) args
                    return (argumentExpression, retTyp)
              (TokenInfo (Number num) _ start end) ->
                if null (mostGeneralUnifier (TypeCon TypeInt) goal)
                  then fail "Int does not match"
                  else return (WithSimplePos start end (Int num), TypeCon TypeInt)
              _ -> generateExpressionSuggestion typeEnv goal
          else fail "Not Enough tokens to satisfy goal!"