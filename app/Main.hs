module Main where

import Data.Either (partitionEithers)
import Data.List (intersperse)
import qualified Data.List as List
import qualified Data.Map as Map
import Lexer.LexerRunner
import Parser.Parser
import Parser.ParserBase
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
  sourceString <- readFile "./test programs/test.hs"
  let maybeParsedTokens = runLexer sourceString
  case maybeParsedTokens of
    Left errorMsg -> putStrLn errorMsg
    Right parsedTokens ->
      let sections = sanitizeSections $ splitIntoSections parsedTokens
          parsedMaybeSections = List.map (parse pSection sourceString . tokensToParsableString sourceString) sections
          (parsedErrors, parsedSections) = partitionEithers parsedMaybeSections
          inferedTypes = inferTypeEnvironment standardTypeEnv (map (\(WithSimplePos _ _ x) -> x) parsedSections)
       in -- mapM_ (print . tokensToString sourceString ) sections
          sequence_ $
            (map (print) sections ++ intersperse (putStrLn "------------------------------------") (map (parseTest pSection . tokensToParsableString sourceString) sections))
              ++ [putStrLn "\n\n-----------------Types:------------------",
              print inferedTypes]

-- parseTest (pSum <* eof) (tokensToParsableString sourceString parsedTokens)
