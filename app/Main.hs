module Main where

import Data.List (intersperse)
import Lexer.LexerRunner
import Parser.Parser
import Parser.ParserBase
import Text.Megaparsec

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

main :: IO ()
main = do
  putStrLn "\n\n\n\n"
  sourceString <- readFile "./test programs/test.hs"
  let maybeParsedTokens = runLexer sourceString
  case maybeParsedTokens of
    Left errorMsg -> putStrLn errorMsg
    Right parsedTokens ->
      let sections = sanitizeSections $ splitIntoSections parsedTokens
       in -- printList sections
          sequence_ $ intersperse (putStrLn "------------------------------------") $ map (parseTest pFunctionDefinition . tokensToParsableString sourceString) sections

-- parseTest (pSum <* eof) (tokensToParsableString sourceString parsedTokens)
