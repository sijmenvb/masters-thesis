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
  deriving ( Eq)

instance Show TokenInfo where 
  show tok = show $ token_type tok

instance Ord TokenInfo where
  TokenInfo {token_type = x} <= TokenInfo {token_type = y} = x <= y -- just let it depend on the ord of the token

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
  = Name
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
  | Dedent
  | Comment
  deriving (Eq, Show, Ord)