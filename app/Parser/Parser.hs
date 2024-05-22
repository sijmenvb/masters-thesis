{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parser.Parser where

import Lexer.Tokens
import Parser.ParserBase
import Parser.Types
import Text.Megaparsec

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

-- to remove leading enters and remove empty/whitespace only sections.
sanitizeSections :: [[TokenInfo]] -> [[TokenInfo]]
sanitizeSections (list@(tok1: toks): rest) = case token_type tok1 of
    Newline -> sanitizeSections (toks :rest)
    _ -> list : sanitizeSections rest
sanitizeSections ([] : rest) = sanitizeSections rest
sanitizeSections x = x

nameToString :: Parser (WithSimplePos Lexer.Tokens.Token) -> Parser (WithSimplePos String)
nameToString nametoken = do 
  WithSimplePos start end (Name name) <- nametoken
  return $ WithSimplePos start end name

pFunctionDefinition :: Parser (WithSimplePos FunctionDefinition)
pFunctionDefinition = do
  WithSimplePos start _ (Name name) <- pName
  labels <- many ( nameToString pName)
  _ <- pToken EqualsSign
  expr@(WithSimplePos _ end _) <- pExpr
  return $ WithSimplePos start end $ FunctionDefinition name labels expr

pExpr :: Parser (WithSimplePos Expr)
pExpr =
  let pBool =
        keepPos (\_ -> Bool True) <$> pToken TrueToken
          <|> keepPos (\_ -> Bool False) <$> pToken FalseToken
      pExprInt = keepPos Int <$> pInt
      pParentheses = do
        WithSimplePos start _ _ <- pToken Lpar
        expr <- pExpr
        WithSimplePos _ end _ <- pToken Rpar
        return $ WithSimplePos start end $ Parentheses expr
      pLabel = pName
   in pParentheses <|> pBool <|> pExprInt
