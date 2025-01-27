{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Parser.Parser where

import Lexer.Tokens
import Parser.ParserBase
import Parser.Types
import Text.Megaparsec
import Data.Foldable

rightAssociative :: (WithSimplePos a -> WithSimplePos a -> WithSimplePos a) -> WithSimplePos a -> [WithSimplePos a] -> WithSimplePos a
rightAssociative constructor first (x1:x2:xs) = constructor first (rightAssociative constructor x1 (x2:xs))
rightAssociative constructor first [x1] = constructor first x1
rightAssociative _ first [] = first

liftFunToWithSimplePosFun :: (a -> a -> a) -> (WithSimplePos a -> WithSimplePos a -> WithSimplePos a)
liftFunToWithSimplePosFun constructor ( WithSimplePos start _ x1)  (WithSimplePos _ end x2) = WithSimplePos start end (constructor x1 x2)

splitIntoSections :: [TokenInfo] -> [[TokenInfo]]
splitIntoSections listIn = map reverse $ splitIntoSections' [] 0 listIn
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

-- to remove leading enters and remove empty/whitespace only sections. (and trailing nextLines)
sanitizeSections :: [[TokenInfo]] -> [[TokenInfo]]
sanitizeSections sections = let
  sanitizeSections2 (list@(tok1 : toks) : rest) = case token_type tok1 of
    Newline -> sanitizeSections (toks : rest)
    _ -> list : sanitizeSections rest
  sanitizeSections2 ([] : rest) = sanitizeSections rest
  sanitizeSections2 x = x
  removeTrailing tok = reverse . dropWhile (\x -> tok == token_type x) . reverse

  removeComments = filter (not . isCommentOrNewlineAfterComment . token_type)

  isCommentOrNewlineAfterComment :: Lexer.Tokens.Token -> Bool
  isCommentOrNewlineAfterComment NewlineAfterComment = True
  isCommentOrNewlineAfterComment (Comment _)             = True
  isCommentOrNewlineAfterComment _                   = False

  in map (removeTrailing Newline)
    . sanitizeSections2
    -- . map removeComments
     $ sections

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
pType = do
  firstType <- pSimpleType
  rest <-
    many
      ( do
          _ <- pToken RArrow
          pWhiteSpace
          pSimpleType
      )
  return $ rightAssociative (liftFunToWithSimplePosFun TypeArrow) firstType rest

pSimpleType :: Parser (WithSimplePos Type)
pSimpleType =
  let pTypeInt = keepPos (\_ -> TypeCon TypeInt) <$> pString "Int"
      pTypeBool = keepPos (\_ -> TypeCon TypeBool) <$> pString "Bool"
      pTypeList = keepPos (\_ -> TypeCon TypeList) <$> pString "List"
      pTypePair = keepPos (\_ -> TypeCon TypePair) <$> pString "Pair"
      pTypeChar = keepPos (\_ -> TypeCon TypeChar) <$> pString "Char"
   in pTypeInt <|>pTypeChar <|> pTypeBool <|> pTypeList <|> pTypePair

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
        arguments <- some pName
        pWhiteSpace
        _ <- pToken RArrow
        pWhiteSpace
        expr <- pExpr
        pWhiteSpace
        buildLambdaExpression start arguments expr 
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
      applicationFold (firstItem : rest) = foldl' buildApplication firstItem rest
      applicationFold _ = error "unreachable state, some should guarantee at least one item in the list."
   in applicationFold <$> some (pParentheses <|> pBool <|> pExprInt <|> pLabel <|> pLambda <|> pLetExpr)



buildLambdaExpression ::  (Int,Int) -> [WithSimplePos Lexer.Tokens.Token] -> WithSimplePos Expr -> Parser (WithSimplePos Expr)
buildLambdaExpression start [] expr@(WithSimplePos _ end _) = fail "internal buildLambdaExpression can never get an empty list"
buildLambdaExpression start [WithSimplePos _ _ (Name labelName)] expr@(WithSimplePos _ end _) = return (WithSimplePos start end $ LambdaAbstraction labelName expr)
buildLambdaExpression start (WithSimplePos _ _ (Name labelName) : rest@(WithSimplePos newStart _ _ : xs)) expr@(WithSimplePos _ end _) = do
  finalExpr <- buildLambdaExpression newStart rest expr
  return (WithSimplePos start end $ LambdaAbstraction labelName finalExpr)
