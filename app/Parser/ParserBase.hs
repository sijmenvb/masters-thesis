{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser.ParserBase where

import qualified Data.List as DL
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import qualified Data.Set as Set
import Data.Text (length, unpack)
import Data.Void
import Lexer.Tokens as Tokens
import Text.Megaparsec

data WithPos a = WithPos
  { startPos :: SourcePos,
    endPos :: SourcePos,
    tokenLength :: Int,
    tokenVal :: a
  }
  deriving (Eq, Ord, Show)

data WithSimplePos a = WithSimplePos
  { start_pos :: (Int, Int),
    end_pos :: (Int, Int),
    value :: a
  }
  deriving (Show)

keepPos :: a1 -> WithSimplePos a2 -> WithSimplePos a1
keepPos val (WithSimplePos start end _) = WithSimplePos start end val

data MyStream = MyStream
  { myStreamInput :: String, -- for showing offending lines
    unMyStream :: [WithPos TokenInfo]
  }

tokensToParsableString :: String -> [TokenInfo] -> MyStream
tokensToParsableString source tokens = MyStream source (map liftTokenInfo tokens)

liftTokenInfo :: TokenInfo -> WithPos TokenInfo
liftTokenInfo tok@TokenInfo {start_pos = (lineBegin, columnBegin), end_pos = (lineEnd, columnEnd), token_string = str} =
  WithPos (SourcePos "" (mkPos lineBegin) (mkPos columnBegin)) (SourcePos "" (mkPos lineEnd) (mkPos columnEnd)) (Data.Text.length str) tok

instance Stream MyStream where
  type Token MyStream = WithPos TokenInfo
  type Tokens MyStream = [WithPos TokenInfo]

  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = Prelude.length
  chunkEmpty Proxy = null
  take1_ (MyStream _ []) = Nothing
  take1_ (MyStream str (t : ts)) =
    Just
      ( t,
        MyStream (drop (tokensLength pxy (t :| [])) str) ts
      )
  takeN_ n (MyStream str s)
    | n <= 0 = Just ([], MyStream str s)
    | null s = Nothing
    | otherwise =
        let (x, s') = splitAt n s
         in case NE.nonEmpty x of
              Nothing -> Just (x, MyStream str s')
              Just nex -> Just (x, MyStream (drop (tokensLength pxy nex) str) s')
  takeWhile_ f (MyStream str s) =
    let (x, s') = DL.span f s
     in case NE.nonEmpty x of
          Nothing -> (x, MyStream str s')
          Just nex -> (x, MyStream (drop (tokensLength pxy nex) str) s')

instance VisualStream MyStream where
  showTokens Proxy =
    unwords
      . NE.toList
      . fmap (showMyToken . tokenVal)
  tokensLength Proxy xs = sum (tokenLength <$> xs)

instance TraversableStream MyStream where
  reachOffset o PosState {..} =
    ( Just (prefix ++ restOfLine),
      PosState
        { pstateInput =
            MyStream
              { myStreamInput = postStr,
                unMyStream = post
              },
          pstateOffset = max pstateOffset o,
          pstateSourcePos = newSourcePos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix = prefix
        }
    )
    where
      prefix =
        if sameLine
          then pstateLinePrefix ++ preLine
          else preLine
      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
      newSourcePos =
        case post of
          [] -> case unMyStream pstateInput of
            [] -> pstateSourcePos
            xs -> endPos (last xs)
          (x : _) -> startPos x
      (pre, post) = splitAt (o - pstateOffset) (unMyStream pstateInput)
      (preStr, postStr) = splitAt tokensConsumed (myStreamInput pstateInput)
      preLine = reverse . takeWhile (/= '\n') . reverse $ preStr
      tokensConsumed =
        case NE.nonEmpty pre of
          Nothing -> 0
          Just nePre -> tokensLength pxy nePre
      restOfLine = takeWhile (/= '\n') postStr

pxy :: Proxy MyStream
pxy = Proxy

showMyToken :: TokenInfo -> String
showMyToken (TokenInfo _ str _ _) = unpack str

type Parser = Parsec Void MyStream

liftMyToken :: TokenInfo -> WithPos TokenInfo
liftMyToken myToken = WithPos pos pos 0 myToken
  where
    pos = initialPos ""

pToken :: Tokens.Token -> Parser (WithSimplePos Tokens.Token)
pToken c = do
  (TokenInfo {token_type = tok, start_pos = start_pos, end_pos = end_pos}) <- token test (Set.singleton . Tokens . nes . liftMyToken $ liftToken c)
  return $ WithSimplePos start_pos end_pos tok
  where
    test (WithPos {tokenVal = x}) =
      if token_type x == token_type (liftToken c)
        then Just x
        else Nothing
    nes x = x :| []

pInt :: Parser (WithSimplePos Int)
pInt = token test Set.empty <?> "integer"
  where
    test (WithPos _ _ _ TokenInfo {token_type = (Number n), start_pos = start_pos, end_pos = end_pos}) =
      Just $ WithSimplePos start_pos end_pos n
    test _ = Nothing

liftToken :: Tokens.Token -> TokenInfo
liftToken tok = TokenInfo tok "" (0, 0) (0, 0)

pSum :: Parser (WithSimplePos (Int, Int))
pSum = do
  WithSimplePos start _ a <- pInt
  _ <- pToken Plus
  WithSimplePos _ end b <- pInt
  return (WithSimplePos start end (a, b))

exampleStream :: MyStream
exampleStream =
  MyStream
    "5 + 6"
    [ at 1 1 $ liftToken (Number 5),
      at 1 3 $ liftToken Plus, -- (1)
      at 1 5 $ liftToken (Number 6)
    ]
  where
    at l c = WithPos (at' l c) (at' l (c + 1)) 2
    at' l c = SourcePos "" (mkPos l) (mkPos c)

myTest :: IO ()
myTest = parseTest (pSum <* eof) exampleStream