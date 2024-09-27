{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Suggestions.Suggestions where

import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Void
import Lexer.Tokens (Token (..), TokenInfo (TokenInfo, token_type))
import Parser.ParserBase
import Parser.Types
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

generateSuggestion :: InferenceState -> TypeEnvironment -> [TokenInfo] -> MaybeError Section
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
      argumentTypeVars = Map.fromList $ indexedMap (\index (WithSimplePos _ _ name) -> (name,FreshVar $ state + index)) arguments
      expressionTokens = (drop 1 . dropWhile (\tokenInfo -> token_type tokenInfo /= EqualsSign)) tokens
   in do
        expr <- generateExpressionSuggestion (Map.union argumentTypeVars typeEnv) expressionTokens
        return $ FunctionDefinition functionName arguments expr
        <?> "could not generate a suggestion for " ++ show functionName ++ " " ++ show arguments ++ " " ++ show expressionTokens

generateExpressionSuggestion :: TypeEnvironment -> [TokenInfo] -> MaybeError (WithSimplePos Expr)
generateExpressionSuggestion typeEnv tokens = Error "TODO: implement me"