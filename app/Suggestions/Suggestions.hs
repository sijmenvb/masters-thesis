{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Suggestions.Suggestions where

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Void
import Lexer.Tokens (Token (..), TokenInfo (TokenInfo, token_type))
import Parser.ParserBase
import Parser.Types
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import TypeInference.TypeInference

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

generateSuggestion :: TypeEnvironment -> [TokenInfo] -> MaybeError Section
generateSuggestion typeEnv tokens =
  let name = getSectionName tokens
      getArguments :: [TokenInfo] -> [WithSimplePos LabelIdentifier]
      getArguments list = case list of
        (TokenInfo EqualsSign _ _ _ : xs) -> []
        (TokenInfo (Name name) _ start end : xs) -> WithSimplePos start end name : getArguments xs
        (TokenInfo tok _ start end : xs) -> WithSimplePos start end ("weird token " ++ show tok) : getArguments xs
        _ -> []
      arguments = getArguments (drop 1 tokens)
      expressionTokens = (drop 1 . dropWhile (\tokenInfo -> token_type tokenInfo /= EqualsSign)) tokens
   in do
        expr <- runSugesstionBuilder (generateExpressionSuggestion typeEnv (FreshVar 99)) expressionTokens
        return $ FunctionDefinition name arguments expr

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

generateExpressionSuggestion :: TypeEnvironment -> Type -> SuggestionBuilder (WithSimplePos Expr)
generateExpressionSuggestion typeEnv goal = do
  hasTok <- hasTokens
  if hasTok
    then do
      tok <- popToken
      case tok of
        (TokenInfo (Name name) _ start end) -> do
          typ <- liftError (Map.lookup name typeEnv) <?> "could not find " ++ name ++ " in type enviornment"
          hasTok2 <- hasTokens
          if hasTok2
            then
              if null (mostGeneralUnifier typ goal)
                then fail "could not build up type"
                else return $ WithSimplePos start end (Label name)
            else case typ of
              TypeArrow argTyp retTyp -> do
                argExpr <- generateExpressionSuggestion typeEnv argTyp
                return $ WithSimplePos start end (Application (WithSimplePos start end (Label name)) argExpr)
              _ -> fail "too many arguments"
        _ -> generateExpressionSuggestion typeEnv goal
    else
      fail "Not Enough tokens to satisfy goal!"