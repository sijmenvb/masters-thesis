module Main where

import Data.Either (partitionEithers)
import Data.List (intersperse)
import qualified Data.List as List
import qualified Data.Map as Map
import Lexer.LexerRunner
import Parser.Parser
import Parser.ParserBase
import Suggestions.Suggestions
import Text.Megaparsec
import TypeInference.TypeInference (TypeEnvironment)
import TypeInference.TypeInferenceUtil

{-
main :: IO ()
main = do
  putStrLn "\n\n\n\n"
  sourceString <- readFile "./test programs/simple.hs"
  runAndPrettyPrintLexer sourceString
-}

-- actually prints nextlines
printList :: Show a => [a] -> IO ()
printList = mapM_ (putStrLn . show)

standardTypeEnv :: TypeEnvironment
standardTypeEnv = Map.empty

main :: IO ()
main = do
  putStrLn "\n\n\n\n"
  sourceString <- readFile "./test programs/suggestions.hs"
  let maybeParsedTokens = runLexer sourceString
  case maybeParsedTokens of
    Left errorMsg -> putStrLn errorMsg
    Right parsedTokens ->
      let sections = sanitizeSections $ splitIntoSections parsedTokens
          parsedMaybeSections = List.map (parse pSection sourceString . tokensToParsableString sourceString) sections
          (parsedErrors, parsedSections) = partitionEithers parsedMaybeSections
          parseProblemsBundle = getParseProblems parsedMaybeSections sections
          (inferredTypes, inferenceProblems,state) = inferTypeEnvironment standardTypeEnv (map (\(WithSimplePos _ _ x) -> x) parsedSections)
          inferenceProblemsBundle = matchProblems sections inferenceProblems
       in sequence_ $
            [putStrLn "-----------------tokens:------------------"]
              ++ map print sections
              ++ [putStrLn "\n\n-----------------expressions:------------------"]
              ++ intersperse (putStrLn "------------------------------------") (map print parsedSections)
              ++ [putStrLn "\n-----------------expression errors:------------------"]
              ++ intersperse (putStrLn "------------------------------------") (map (putStrLn . errorBundlePretty) parsedErrors)
              ++ [putStrLn "\n\n-----------------Types:------------------"]
              ++ map print (Map.toList inferredTypes)
              ++ [putStrLn "\n-----------------Type errors:------------------"]
              ++ map print inferenceProblems
              ++ [putStrLn "\n-----------------Suggestions:------------------"]
              ++ map (print . generateSuggestion state inferredTypes . fst) (reverse $ parseProblemsBundle ++ inferenceProblemsBundle)
              ++ [putStrLn "\n\n"]
