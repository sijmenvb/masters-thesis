{
{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE RecordWildCards#-}

module Lexer.Lexer where

import Lexer.LexerUtil
import Lexer.Tokens
import qualified Data.Text as T
import qualified Data.List as L

}

-- %wrapper "basic"

$digit = 0-9   -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$white_no_nl = [\ \t]

@identifier = $alpha [$alpha $digit \_]*

@decnumber = (0(_?0)* | [1-9](_?[0-9])*)

@number = @decnumber

$newline = [\r \n]
$not_newline = ~$newline
@commentline = (($white_no_nl)*  \-\- ($not_newline)*)




tokens :-
      @number {numberAction}
      @identifier {action Name}
      \n$white_no_nl* {startWhite}
       $white_no_nl+ ;   -- ignote this since we only care up significant white spaces (leading white spaces)
      "("   { action Lpar }
      ")"   { action Rpar }
      "["   { action Lsqb }
      "]"   { action Rsqb }
      "{"   { action Lbrace }
      "}"   { action Rbrace }
      "->"  { action RArrow }
      "."   { action Dot }
      "+"   { action Plus }
      "-"   { action Minus }
      "*"   { action Star }
      "::"  { action DoubleColon }
      "="   { action EqualsSign }
      @commentline  { commentAction }
      "{-"  { multiLineCommentAction }
{

-- adapted from https://www.haskell.org/alex/doc/html/wrappers.html
-- alexScanTokens :: String -> [TokenInfo]
lexer :: Alex TokenInfo
lexer = do
  userState <- alexGetUserState
  case userStatePendingTokens userState of
    t:ts -> do
        alexSetUserState $ userState {userStatePendingTokens=ts}
        return t
    [] -> do
      inp <- alexGetInput
      sc <- alexGetStartCode
      case alexScan inp sc of
        AlexEOF -> alexEOF
        AlexError ((AlexPosn _ line column),_,_,_) -> alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
        AlexSkip  inp__' _len -> do
          alexSetInput inp__'
          lexer
        AlexToken inp' len action -> do
          alexSetInput inp'
          action inp len


-- adopted from language-python
lexerFold :: Alex [TokenInfo]
lexerFold = loop []
  where
    loop toks = do
      token_info@TokenInfo {..} <- lexer
      case token_type of
        EOF -> return $ L.reverse toks
        _ -> loop (token_info : toks)

-- from generated Alex file
runAlex :: String -> Alex a -> Either String a
runAlex inp (Alex f) =
  case f
    ( AlexState
        { alex_pos = alexStartPos,
          alex_inp = inp,
          alex_chr = '\n',
          alex_bytes = [],
          alex_ust = alexInitUserState,
          alex_scd = 0
        }
    ) of
    Left msg -> Left msg
    Right (_, a) -> Right a

}