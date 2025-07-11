{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Lexer.Tokens where

import qualified Data.Text as T
import RIO

data TokenInfo = TokenInfo
  { token_type :: Token,
    token_string :: T.Text,
    start_pos :: (Int, Int),
    end_pos :: (Int, Int)
  }
  deriving (Eq)

instance Show TokenInfo where
  show tok = show $ token_type tok

instance Ord TokenInfo where
  TokenInfo {token_type = x} <= TokenInfo {token_type = y} = x <= y -- just let it depend on the ord of the token


showTokenInfoWithPosition :: TokenInfo -> String
showTokenInfoWithPosition (TokenInfo tok _ (startLine, startCol) (endLine, endCol)) = prettyShow tok  ++ "[" ++ show startLine ++ ":" ++ show startCol ++ "-" ++ show endLine ++ ":" ++ show endCol ++ "]"

--- Used for testing and
tokenInfoTuple :: TokenInfo -> (String, String, (Int, Int), (Int, Int))
tokenInfoTuple tok =
  (,,,)
    <$> T.unpack . T.toUpper . T.pack . show . token_type
    <*> T.unpack . token_string'
    <*> start_pos
    <*> end_pos
    $ tok
  where
    token_string' tok2 =
      let token = token_string tok2
          token' = T.replace (T.pack "\\") (T.pack "\\\\") token
          token'' = T.replace (T.pack "\n") (T.pack "\\n") token'
       in token''

-- Used for testing
tokenInfoToken :: TokenInfo -> String
tokenInfoToken = T.unpack . T.toUpper . T.pack . show . token_type

-- TODO: implement the rest of these in lexer.x as well.
data Token
  = Name String
  | Number Int
  | String
  | Op
  | Lpar
  | Rpar
  | Lsqb
  | Rsqb
  | Lbrace
  | Rbrace
  | Dot
  | Colon
  | Comma
  | Semi
  | Plus
  | Minus
  | RArrow
  | Star
  | DoubleColon
  | EqualsSign
  | Newline
  | NewlineAfterComment
  | EOF
  | Indent
  | InternalAddIndentToPrevious -- used when finding too many indents.
  | Dedent
  | Comment String
  | TrueToken
  | FalseToken
  | Lambda
  | Let
  | In
  deriving (Eq, Show, Ord)

-- prettyShow generated by chatgpt
-- prettyShow used in displaying errors.
prettyShow :: Token -> String
prettyShow token = case token of
  Name s -> "Name: " ++ s
  Number n -> "Number: " ++ show n
  String -> "String"
  Op -> "Operator"
  Lpar -> "("
  Rpar -> ")"
  Lsqb -> "["
  Rsqb -> "]"
  Lbrace -> "{"
  Rbrace -> "}"
  Dot -> "."
  Colon -> ":"
  Comma -> "comma"
  Semi -> ";"
  Plus -> "+"
  Minus -> "-"
  RArrow -> "->"
  Star -> "*"
  DoubleColon -> "::"
  EqualsSign -> "="
  Newline -> "Newline"
  NewlineAfterComment -> "Newline"
  EOF -> "EOF"
  Indent -> "Indent"
  Dedent -> "Dedent"
  Comment str-> "Comment: " ++ str
  TrueToken -> "True"
  FalseToken -> "False"
  Lambda -> "λ" -- TODO: this might cause confusion since it's hard to type. it also accepts \ maybe change it to "\\"
  Let -> "let"
  In -> "in"

stringRepeat :: Int -> String -> String
stringRepeat n string = concat $ replicate n string

showExact :: Token -> String
showExact token =
  case token of
    Name s -> s
    Number n -> show n
    String -> "String"
    Op -> "Operator"
    Lpar -> "("
    Rpar -> ")"
    Lsqb -> "["
    Rsqb -> "]"
    Lbrace -> "{"
    Rbrace -> "}"
    Dot -> "."
    Colon -> ":"
    Comma -> ","
    Semi -> ";"
    Plus -> "+"
    Minus -> "-"
    RArrow -> "->"
    Star -> "*"
    DoubleColon -> "::"
    EqualsSign -> "="
    Newline -> "\n"
    NewlineAfterComment -> "\n"
    EOF -> "EOF"
    Indent -> "    "
    Dedent -> ""
    Comment str -> str
    TrueToken -> "True"
    FalseToken -> "False"
    Lambda -> "\\"
    Let -> "let"
    In -> "in"
    _ -> show token

recreateOriginalShow :: [Token] -> String
recreateOriginalShow tokensIn =
  let recreateOriginalShow2 :: Int -> [Token] -> String
      recreateOriginalShow2 indentLevel tokens = case tokens of
        Newline : Dedent : tokensRest -> recreateOriginalShow2 (indentLevel - 1) (Newline:tokensRest)
        [] -> ""
        Newline : tokensRest -> "\n" ++ stringRepeat indentLevel (showExact Indent) ++ recreateOriginalShow2 indentLevel tokensRest
        Indent : tokensRest -> recreateOriginalShow2 (indentLevel + 1) tokensRest
        Dedent : tokensRest -> recreateOriginalShow2 (indentLevel - 1) tokensRest
        tok : Rpar : tokensRest -> showExact tok ++ recreateOriginalShow2 indentLevel (Rpar : tokensRest)
        Lpar : tokensRest -> showExact Lpar ++ recreateOriginalShow2 indentLevel tokensRest
        Lambda : tokensRest -> showExact Lambda ++ recreateOriginalShow2 indentLevel tokensRest
        tok : tokensRest -> showExact tok ++ " " ++ recreateOriginalShow2 indentLevel tokensRest
   in recreateOriginalShow2 0 tokensIn