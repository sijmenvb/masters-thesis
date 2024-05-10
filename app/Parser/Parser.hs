module Parser.Parser where

import Lexer.Tokens
import Parser.ParserBase

splitIntoSections :: [TokenInfo] -> [[TokenInfo]]
splitIntoSections list = map reverse $ splitIntoSections' [] 0 list
  where
    splitIntoSections' :: [TokenInfo] -> Int -> [TokenInfo] -> [[TokenInfo]]
    splitIntoSections' acc depth (tok1 : tok2 : list) = case (token_type tok1, token_type tok2, depth) of
      (Newline, Indent, _) -> splitIntoSections' (tok1 : tok2 : acc) (depth + 1) (list)
      (Indent, _, _) -> splitIntoSections' (tok1 : acc) (depth + 1) (tok2 : list)
      (Dedent, _, _) -> splitIntoSections' (tok1 : acc) (depth - 1) (tok2 : list)
      (Newline, _, 0) -> acc : splitIntoSections' ([tok1]) (depth) (tok2 : list)
      (_, _, 0) -> splitIntoSections' (tok1 : acc) (depth) (tok2 : list)
      _ -> splitIntoSections' (tok1 : acc) depth (tok2 : list)
    splitIntoSections' acc _ [tok] = [tok : acc]
    splitIntoSections' acc _ [] = [acc]