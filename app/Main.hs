module Main where
import Parser
import Lexer.LexerRunner 

main :: IO ()
main = do
  putStrLn "\n\n\n\n"
  contents <- readFile "./test programs/simple.hs"
  runAndPrettyPrintLexer contents


fun :: Num a => a -> a
fun x = x + 10