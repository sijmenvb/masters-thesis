module Suggestions.TokenDifference where

import Control.Monad.State as State
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.Map as Map
import Lexer.Tokens
import Parser.ParserBase (WithSimplePos (WithSimplePos))
import Parser.Types (Expr (..), Section (..))
import System.Console.ANSI

data Action a
  = Keep a
  | Add a
  | Remove a

forgetAction action = case action of
  Keep x -> x
  Add x -> x
  Remove x -> x

getActionColor :: Action a -> String
getActionColor action = case action of
  Keep _ -> whiteColor
  Add _ -> greenColor
  Remove _ -> redColor

recreateOriginalWithDifferencesShow :: [Action Token] -> String
recreateOriginalWithDifferencesShow tokensIn =
  let recreateOriginalShow2 :: Int -> [(Token, String, Action Token)] -> String
      recreateOriginalShow2 indentLevel tokens = case tokens of
        [] -> ""
        (Newline, color, Add _) : tokensRest -> color ++ "newline\n" ++ resetColor ++ stringRepeat indentLevel (showExact Indent) ++ recreateOriginalShow2 indentLevel tokensRest
        (Newline, _, _) : tokensRest -> "\n" ++ stringRepeat indentLevel (showExact Indent) ++ recreateOriginalShow2 indentLevel tokensRest
        (Indent, _, _ ) : tokensRest -> recreateOriginalShow2 (indentLevel + 1) tokensRest
        (Dedent, _, _) : tokensRest -> recreateOriginalShow2 (indentLevel - 1) tokensRest
        (tok, color,_) : rpar@(Rpar, _,_) : tokensRest -> color ++ showExact tok ++ resetColor ++ recreateOriginalShow2 indentLevel (rpar : tokensRest)
        (Lpar, color,_) : tokensRest -> color ++ showExact Lpar ++ resetColor ++ recreateOriginalShow2 indentLevel tokensRest
        (tok, color,_) : tokensRest -> color ++ showExact tok ++ resetColor ++ " " ++ recreateOriginalShow2 indentLevel tokensRest
   in recreateOriginalShow2 0 $ Prelude.map (\tok -> (forgetAction tok, getActionColor tok,tok)) tokensIn

instance Show a => Show (Action a) where
  show (Keep a) = whiteColor ++ show a ++ resetColor
  show (Add a) = greenColor ++ show a ++ resetColor
  show (Remove a) = redColor ++ show a ++ resetColor

data ExtendedTokens
  = Require Token
  | Optional Token (Maybe Int)
  deriving (Ord,Eq)

-- Helper function to reset the color
resetColor :: String
resetColor = setSGRCode [Reset]

-- ANSI codes for white and gray
whiteColor :: String
whiteColor = setSGRCode [SetColor Foreground Vivid White]

grayColor :: String
grayColor = setSGRCode [SetColor Foreground Dull Black]

greenColor :: String
greenColor = setSGRCode [SetColor Foreground Vivid Green]

redColor :: String
redColor = setSGRCode [SetColor Foreground Vivid Red]

instance Show ExtendedTokens where
  show (Require token) =
    whiteColor ++ show token ++ resetColor
  show (Optional token num) =
    grayColor ++ show token ++ show num ++ resetColor

{- modifyTokenString :: WithSimplePos Expr -> State [TokenInfo] [Action TokenInfo]
modifyTokenString expr =
  let require :: Token -> State [TokenInfo] [Action TokenInfo]
      require tok = do
        tokens <- get
        case tokens of
          [] -> return [Add $ TokenInfo tok Data.Text.empty (-1, -1) (-1, -1)]
          (input@(TokenInfo inTok _ _ _) : xs) | tok == inTok -> do
            put xs
            return [ Keep input]
          (input@(TokenInfo inTok text start end) : xs) -> return $ Add $ TokenInfo tok text start end

      getNextTok :: State [TokenInfo] (Maybe TokenInfo)
      getNextTok = do
        tokens <- Control.Monad.State.get
        case tokens of
          (x : xs) -> return $ Just x
          _ -> return Nothing
   in case expr of
        (WithSimplePos start end (Parentheses expr)) -> do
          tok <- getNextTok
          lPar <- require Lpar
          exprToks <- modifyTokenString expr
          rPar <- require Rpar
          return $ lPar ++ exprToks ++ rPar -}

testfunc :: Section -> ([ExtendedTokens], Int)
testfunc sect = runState (createTargetTokensFromSection sect) 0

createTargetTokensFromSection :: Section -> State Int [ExtendedTokens]
createTargetTokensFromSection section =
  case section of
    FunctionDefinition name vars expr ->
      do
        exprTokens <- createTargetTokensFromExpr expr
        let varTokens = Prelude.map (\(WithSimplePos _ _ var) -> Require (Name var)) vars
        return $ Require (Name name) : varTokens ++ (Require EqualsSign : exprTokens)
    FunctionType name typ -> return []

createTargetTokensFromExpr :: WithSimplePos Expr -> State Int [ExtendedTokens]
createTargetTokensFromExpr expr =
  let getId :: State Int Int
      getId =
        do
          currentId <- get
          put (currentId + 1)
          return currentId
   in case expr of
        (WithSimplePos _ _ (Parentheses expr1)) ->
          do
            exprTokens <- createTargetTokensFromExpr expr1
            return $ Require Lpar : exprTokens ++ [Require Rpar]
        (WithSimplePos _ _ (Application expr1 expr2)) ->
          do
            bracketId <- getId
            indentId <- getId
            expr1Tokens <- createTargetTokensFromExpr expr1
            expr2Tokens <- createTargetTokensFromExpr expr2
            let argumentTokens =
                  case expr2 of
                    (WithSimplePos _ _ (Application _ _)) -> Require Lpar : expr2Tokens ++ [Require Rpar]
                    _ -> expr2Tokens
            return $ Optional Lpar (Just bracketId) : expr1Tokens ++ [Optional Indent (Just indentId), Optional Newline (Just indentId)] ++ argumentTokens ++ [Optional Rpar (Just bracketId), Optional Newline Nothing, Optional Dedent (Just indentId)] -- TODO: fix early dedent issue
        (WithSimplePos _ _ (Label name)) ->
          return [Require (Name name)]
        (WithSimplePos _ _ (Int num)) ->
          return [Require (Number num)]
        (WithSimplePos _ _ (Bool b)) ->
          return [if b then Require TrueToken else Require FalseToken]
        _ -> undefined

needsParentheses :: Expr -> Bool
needsParentheses expr =
  case expr of
    Parentheses _ -> True
    Application _ _ -> True
    LambdaAbstraction _ _ -> True
    _ -> False

-- TODO: use dynamic programming approach since this is unbearably slow.
generateActions :: [ExtendedTokens] -> [Token] -> [Action Token]
generateActions extTokensIn tokensIn =
  -- generateActions2 is heavily inspired by Levenshtein distance, probably not very efficient
  let standardWeight :: Int
      standardWeight = 10

      -- getTokenRemovalWeight used to make removing certain tokens more expensive. for example when you want to swap the number 3 and a parenthesis remove and add the parenthesis not the number.
      getTokenRemovalWeight :: Token -> Int
      getTokenRemovalWeight tok = case tok of
        Name _ -> 15
        Number _ -> 15
        _ -> standardWeight

      -- append is probably a terrible name
      append :: Int -> Action Token -> (Int, [Action Token]) -> (Int, [Action Token])
      append incrementAmmount actionToken (num, list) = (num + incrementAmmount, actionToken : list)

      dynamicGenerateActions2 :: Map Int Bool -> [ExtendedTokens] -> [Token] -> State (Map (Map Int Bool, [ExtendedTokens], [Token]) (Int, [Action Token])) (Int, [Action Token])
      dynamicGenerateActions2 foundTokens extTokens tokens = do
        answers <- get
        case Map.lookup (foundTokens, extTokens, tokens) answers of
          Just x -> pure x
          Nothing -> do
                      x <- generateActions2 foundTokens extTokens tokens
                      answers2 <- get 
                      put $ Map.insert (foundTokens, extTokens, tokens) x answers2
                      return x

      generateActions2 :: Map Int Bool -> [ExtendedTokens] -> [Token] -> State (Map (Map Int Bool, [ExtendedTokens], [Token]) (Int, [Action Token])) (Int, [Action Token])
      generateActions2 foundTokens extTokens tokens =
        case (extTokens, tokens) of
          -- allow for comments
          (_, com@(Comment _)  : tokRest ) -> do
            x <- dynamicGenerateActions2 foundTokens extTokens tokRest
            return $ append 0 (Keep com) x

          -- both lists empty
          ([], []) -> return (0, [])
          -- token stream empty
          (Require tok1 : extTokensRest, []) -> do
            x <- dynamicGenerateActions2 foundTokens extTokensRest []
            return $ append standardWeight (Add tok1) x
          (Optional _ Nothing : extTokensRest, []) -> dynamicGenerateActions2 foundTokens extTokensRest []
          (Optional tok1 (Just id) : extTokensRest, []) ->
            case Map.lookup id foundTokens of
              Just True -> dynamicGenerateActions2 foundTokens (Require tok1 : extTokensRest) tokens
              _ -> dynamicGenerateActions2 foundTokens extTokensRest []
          -- expected token stream empty
          ([], tok2 : tokRest) -> do
            x <- dynamicGenerateActions2 foundTokens [] tokRest
            return $ append (getTokenRemovalWeight tok2) (Remove tok2) x
          -- both streams not empty
          (Require tok1 : extTokensRest, tok2 : tokRest)
            | tok1 == tok2 -> do
              x <- dynamicGenerateActions2 foundTokens extTokensRest tokRest
              return $ append 0 (Keep tok2) x
          (Require tok1 : extTokensRest, tok2 : tokRest) -> do
            x1 <- dynamicGenerateActions2 foundTokens extTokensRest tokRest
            x2 <- dynamicGenerateActions2 foundTokens extTokensRest tokens
            x3 <- dynamicGenerateActions2 foundTokens extTokens tokRest
            return $  minimumBy
              (compare `on` fst)
              [ append (getTokenRemovalWeight tok2) (Remove tok2) $ append standardWeight (Add tok1) x1, -- replace current token by the new one.
                append standardWeight (Add tok1) x2, -- add a missing token
                append (getTokenRemovalWeight tok2) (Remove tok2) x3 -- remove the extra token
              ]
          (Optional tok1 Nothing : extTokensRest, tok2 : tokRest)
            | tok1 == tok2 -> do
              x1 <- dynamicGenerateActions2 foundTokens extTokensRest tokRest
              x2 <- dynamicGenerateActions2 foundTokens extTokensRest tokens
              x3 <- dynamicGenerateActions2 foundTokens extTokens tokRest
              return $  minimumBy
                  (compare `on` fst)
                  [ append 0 (Keep tok2) x1, -- use the optional token.
                    x2, -- do not use the optional token
                    append (getTokenRemovalWeight tok2) (Remove tok2) x3 -- remove the extra token
                  ]
          (Optional _ Nothing : extTokensRest, tok2 : tokRest) -> do
            x1 <- dynamicGenerateActions2 foundTokens extTokensRest tokens
            x2 <- dynamicGenerateActions2 foundTokens extTokens tokRest
            return $ minimumBy
              (compare `on` fst)
              [ x1 , -- do not use the optional token
                append (getTokenRemovalWeight tok2) (Remove tok2) x2  -- remove the extra token
              ]
          (Optional tok1 (Just id) : extTokensRest, tok2 : tokRest) ->
            case (Map.lookup id foundTokens, tok1 == tok2) of
              (Nothing, True) -> do
                x1 <- dynamicGenerateActions2 (Map.insert id False foundTokens) extTokensRest tokens
                x2 <- dynamicGenerateActions2 (Map.insert id True foundTokens) extTokensRest tokRest
                x3 <- dynamicGenerateActions2 foundTokens extTokens tokRest
                return $ minimumBy
                  (compare `on` fst)
                  [ x1 , -- use the optional token.
                    append 0 (Keep tok2) x2, -- do not use the optional token
                    append (getTokenRemovalWeight tok2) (Remove tok2) x3 -- remove the extra token
                  ]
              (Nothing, False) -> do
                x1 <- dynamicGenerateActions2 (Map.insert id False foundTokens) extTokensRest tokens
                x2 <- dynamicGenerateActions2 foundTokens extTokens tokRest
                return $ minimumBy
                  (compare `on` fst)
                  [ x1, -- do not use the optional token
                    append (getTokenRemovalWeight tok2) (Remove tok2) x2  -- remove the extra token
                  ]
              (Just True, _) -> dynamicGenerateActions2 foundTokens (Require tok1 : extTokensRest) tokens -- require the optional token
              (Just False, _) -> dynamicGenerateActions2 foundTokens extTokensRest tokens -- skip the optional token
   in
    snd $ fst $ State.runState (dynamicGenerateActions2 Map.empty extTokensIn tokensIn) Map.empty
