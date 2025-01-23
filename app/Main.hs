module Main where

import Data.Either (partitionEithers)
import Data.List (intersperse)
import qualified Data.List as List
import qualified Data.Map as Map
import Debug.Trace
import Lexer.LexerRunner
import Lexer.Tokens (TokenInfo)
import Parser.Parser
import Parser.ParserBase
import Parser.Types (Problem, Type, TypeEnvironment, getNameFromProblem)
import Suggestions.Suggestions
import Text.Megaparsec
import TypeInference.TypeInference (MaybeError (..))
import TypeInference.TypeInferenceUtil ( inferTypeEnvironment )


-- actually prints nextlines
printList :: Show a => [a] -> IO ()
printList = mapM_ (putStrLn . show)

standardTypeEnv :: TypeEnvironment
standardTypeEnv = Map.empty

main :: IO ()
main = do
  putStrLn "\n\n\n\n"
  let fileName = "./test programs/suggestions.hs"
  sourceString <- readFile fileName
  case runLexer sourceString of
    Left errorMsg -> putStrLn errorMsg
    Right parsedTokens ->
      let sections = sanitizeSections $ splitIntoSections parsedTokens
          parsedMaybeSections = List.map (parse pSection fileName . tokensToParsableString sourceString) sections
          (parsedErrors, parsedSections) = partitionEithers parsedMaybeSections
          parseProblemsBundle = getParseProblems parsedMaybeSections sections
          (inferredTypes, inferenceProblems, state) = inferTypeEnvironment standardTypeEnv (map (\(WithSimplePos _ _ x) -> x) parsedSections)
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
              ++ [putStrLn $ "\n-----------------Suggestions:------------------" ++ show (length (parseProblemsBundle ++ inferenceProblemsBundle))]
              ++ map (makeSuggestion state inferredTypes) (reverse $ parseProblemsBundle ++ inferenceProblemsBundle)
              ++ [putStrLn "\n\nDONE!\n\n"]

makeSuggestion :: Int -> TypeEnvironment -> ([TokenInfo], Problem) -> IO ()
makeSuggestion state inferredTypes problembundle =
  let maybeErr = generateSuggestion state inferredTypes (fst problembundle)
   in case maybeErr of
        Justt (expectedTokens, fixString, diffString, typ, numberOfBranches) ->
          sequence_ 
            [ putStrLn "did you mean:",
              putStrLn fixString,
              putStrLn "--diff:--",
              putStrLn diffString,
              putStrLn $ "--Which has type: " ++ show typ,
              putStrLn $ "-Performance: branched " ++ show numberOfBranches ++ " times.",
              putStrLn "------------------------------------"
            ]
        Error str -> sequence_ [putStrLn $ "Problem generating suggestion for " ++ show (getNameFromProblem $ snd problembundle), putStrLn str, putStrLn "------------------------------------"]