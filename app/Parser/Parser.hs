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
sanitizeSections (list@(tok1 : toks) : rest) = case token_type tok1 of
  Newline -> sanitizeSections (toks : rest)
  _ -> list : sanitizeSections rest
sanitizeSections ([] : rest) = sanitizeSections rest
sanitizeSections x = x

nameToString :: Parser (WithSimplePos Lexer.Tokens.Token) -> Parser (WithSimplePos String)
nameToString nametoken = do
  WithSimplePos start end (Name name) <- nametoken
  return $ WithSimplePos start end name

pSection :: Parser (WithSimplePos Section)
pSection =
  let pFunctionDefinition = do
        WithSimplePos start _ (Name name) <- pName
        labels <- many (nameToString pName)
        _ <- pToken EqualsSign
        pWhiteSpace
        expr@(WithSimplePos _ end _) <- pExpr
        pWhiteSpace
        return $ WithSimplePos start end $ FunctionDefinition name labels expr
      pFunctionType = try $ do
        WithSimplePos start _ (Name name) <- pName
        _ <- pToken DoubleColon
        typ@(WithSimplePos _ end _) <- pType
        pWhiteSpace
        return $ WithSimplePos start end $ FunctionType name typ
   in (pFunctionType <|> pFunctionDefinition) <* pWhiteSpace <* eof

pType :: Parser (WithSimplePos Type)
pType = keepPos (\_ -> TypeCon TypeInt) <$> pString "Int"

pExpr :: Parser (WithSimplePos Expr)
pExpr =
  let pBool =
        keepPos (\_ -> Bool True) <$> pToken TrueToken
          <|> keepPos (\_ -> Bool False) <$> pToken FalseToken
      pExprInt = keepPos Int <$> pInt
      pParentheses = do
        WithSimplePos start _ _ <- pToken Lpar
        pWhiteSpace
        expr <- pExpr
        pWhiteSpace
        WithSimplePos _ end _ <- pToken Rpar
        pWhiteSpace
        return $ WithSimplePos start end $ Parentheses expr
      pLabel = do
        WithSimplePos start end (Name str) <- pName
        pWhiteSpace
        return $ WithSimplePos start end (Parser.Types.Label str)
      pLambda = do 
        WithSimplePos start _ _ <- pToken Lambda
        pWhiteSpace
        WithSimplePos _ _ (Name labelName) <- pName
        pWhiteSpace
        _ <- pToken RArrow
        pWhiteSpace
        expr@(WithSimplePos _ end _) <- pExpr
        pWhiteSpace
        return $ WithSimplePos start end $ LambdaAbstraction labelName expr
      pLetExpr = do
        WithSimplePos start _ _ <- pToken Let
        pWhiteSpace
        WithSimplePos _ _ (Name labelName) <- pName
        pWhiteSpace
        _ <- pToken EqualsSign
        pWhiteSpace
        expr1 <- pExpr
        pWhiteSpace
        _ <- pToken In
        pWhiteSpace
        expr2@(WithSimplePos _ end _) <- pExpr
        pWhiteSpace
        return $ WithSimplePos start end $ LetExpression labelName expr1 expr2
      -- used to construct Application (constructor of Expr)
      applicationFold :: [WithSimplePos Expr] -> WithSimplePos Expr
      applicationFold [x] = x
      applicationFold (firstItem : rest) = foldl buildApplication firstItem rest
      applicationFold _ = error "unreachable state, some should guarantee at least one item in the list."
   in applicationFold <$> some (pParentheses <|> pBool <|> pExprInt <|> pLabel <|> pLambda <|> pLetExpr)
