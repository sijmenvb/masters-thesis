module Suggestions.TokenDifference where

import Control.Monad.State
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

instance Show a => Show (Action a) where
  show (Keep a) = whiteColor ++ show a ++ resetColor
  show (Add a) = greenColor ++ show a ++ resetColor
  show (Remove a) = redColor ++ show a ++ resetColor

data ExtendedTokens
  = Require Token
  | Optional Token (Maybe Int)

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
  show (Optional token _) =
    grayColor ++ show token ++ resetColor

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
            return $ Optional Lpar (Just bracketId) : expr1Tokens ++ [Optional Indent (Just indentId), Optional Newline (Just indentId)] ++ argumentTokens ++ [Optional Rpar (Just bracketId), Optional Newline Nothing, Optional Dedent (Just indentId)]
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

      generateActions2 :: Map Int Bool -> [ExtendedTokens] -> [Token] -> (Int, [Action Token])
      generateActions2 foundTokens extTokens tokens =
        case (extTokens, tokens) of
          -- both lists empty
          ([], []) -> (0, [])
          -- token stream empty
          (Require tok1 : extTokensRest, []) -> append standardWeight (Add tok1) $ generateActions2 foundTokens extTokensRest []
          (Optional _ Nothing : extTokensRest, []) -> generateActions2 foundTokens extTokensRest []
          (Optional tok1 (Just id) : extTokensRest, []) ->
            case Map.lookup id foundTokens of
              Just True -> generateActions2 foundTokens (Require tok1 : extTokensRest) tokens
              _ -> generateActions2 foundTokens extTokensRest []
          -- expected token stream empty
          ([], tok2 : tokRest) ->
            append standardWeight (Remove tok2) $ generateActions2 foundTokens [] tokRest
          -- both streams not empty
          (Require tok1 : extTokensRest, tok2 : tokRest)
            | tok1 == tok2 ->
                append 0 (Keep tok2) $ generateActions2 foundTokens extTokensRest tokRest
          (Require tok1 : extTokensRest, tok2 : tokRest) ->
            minimumBy
              (compare `on` fst)
              [ append (getTokenRemovalWeight tok2) (Remove tok2) $ append standardWeight (Add tok1) $ generateActions2 foundTokens extTokensRest tokRest, -- replace current token by the new one.
                append standardWeight (Add tok1) $ generateActions2 foundTokens extTokensRest tokens, -- add a missing token
                append (getTokenRemovalWeight tok2) (Remove tok2) $ generateActions2 foundTokens extTokens tokRest -- remove the extra token
              ]
          (Optional tok1 Nothing : extTokensRest, tok2 : tokRest)
            | tok1 == tok2 ->
                minimumBy
                  (compare `on` fst)
                  [ append 0 (Keep tok2) $ generateActions2 foundTokens extTokensRest tokRest, -- use the optional token.
                    generateActions2 foundTokens extTokensRest tokens, -- do not use the optional token
                    append (getTokenRemovalWeight tok2) (Remove tok2) $ generateActions2 foundTokens extTokens tokRest -- remove the extra token
                  ]
          (Optional _ Nothing : extTokensRest, tok2 : tokRest) ->
            minimumBy
              (compare `on` fst)
              [ generateActions2 foundTokens extTokensRest tokens, -- do not use the optional token
                append (getTokenRemovalWeight tok2) (Remove tok2) $ generateActions2 foundTokens extTokens tokRest -- remove the extra token
              ]
          (Optional tok1 (Just id) : extTokensRest, tok2 : tokRest) ->
            case (Map.lookup id foundTokens, tok1 == tok2) of
              (Nothing, True) ->
                minimumBy
                  (compare `on` fst)
                  [ generateActions2 (Map.insert id False foundTokens) extTokensRest tokens, -- use the optional token.
                    append 0 (Keep tok2) $ generateActions2 (Map.insert id True foundTokens) extTokensRest tokRest, -- do not use the optional token
                    append (getTokenRemovalWeight tok2) (Remove tok2) $ generateActions2 foundTokens extTokens tokRest -- remove the extra token
                  ]
              (Nothing, False) ->
                minimumBy
                  (compare `on` fst)
                  [ generateActions2 (Map.insert id False foundTokens) extTokensRest tokens, -- do not use the optional token
                    append (getTokenRemovalWeight tok2) (Remove tok2) $ generateActions2 foundTokens extTokens tokRest -- remove the extra token
                  ]
              (Just True, _) -> generateActions2 foundTokens (Require tok1 : extTokensRest) tokens -- require the optional token
              (Just False, _) -> generateActions2 foundTokens extTokensRest tokens -- skip the optional token
   in snd $ generateActions2 Map.empty extTokensIn tokensIn
