module Main where
import Parser.ParserBase
import Lexer.LexerRunner 
import Text.Megaparsec

{- 
main :: IO ()
main = do
  putStrLn "\n\n\n\n"
  sourceString <- readFile "./test programs/simple.hs"
  runAndPrettyPrintLexer sourceString
-}

main :: IO ()
main = do
  putStrLn "\n\n\n\n"
  sourceString <- readFile "./test programs/test.hs"
  let maybeParsedTokens = runLexer sourceString
  case maybeParsedTokens of
    Left errorMsg -> putStrLn errorMsg
    Right parsedTokens ->
      parseTest (pSum <* eof) (tokensToParsableString sourceString parsedTokens)


fun :: Num a => a -> a
fun x = x + 10