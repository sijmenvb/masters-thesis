{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE MultiWayIf#-}

module Lexer.Tokens where

import RIO
import qualified Data.Text as T
import Codec.Binary.UTF8.String as UTF8 (encode)
import qualified Data.List as L

data TokenInfo = TokenInfo {
    token_type:: Token
  , token_string:: T.Text
  , start_pos:: (Int, Int)
  , end_pos:: (Int, Int)
  }
  deriving (Show, Eq)

--- Used for testing and
tokenInfoTuple :: TokenInfo -> (String, String, (Int, Int), (Int, Int))
tokenInfoTuple tok = (,,,) <$>
  T.unpack . T.toUpper . T.pack . show . token_type <*>
  T.unpack . token_string' <*>
  start_pos <*>
  end_pos $ tok
  where
    token_string' tok = let
      token = token_string tok
      token' = T.replace (T.pack "\\") (T.pack "\\\\") token
      token'' = T.replace (T.pack "\n") (T.pack "\\n") token'
      in
        token''

-- Used for testing
tokenInfoToken :: TokenInfo -> String
tokenInfoToken =  T.unpack . T.toUpper . T.pack . show . token_type

--TODO: implement the rest of these in lexer.x as well.
data Token =
      Name
    | Number
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
    deriving (Eq, Show)