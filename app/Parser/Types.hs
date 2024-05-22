module Parser.Types where

import Parser.ParserBase

type FunctionName = String

type LabelIdentifier = String

data Expr
  = Parentheses (WithSimplePos Expr)
  | Application (WithSimplePos Expr) (WithSimplePos Expr)
  | Int Int
  | Bool Bool
  | Label LabelIdentifier

--instance Show Expr generated with chatgpt.
instance Show Expr where
  show (Parentheses expr) = "( " ++ show expr ++ " )"
  show (Application func arg) = show func ++ " " ++ show arg
  show (Int n) = show n
  show (Bool b) = show b
  show (Label label) = label

-- used to store function definitions that look like:
-- FunctionName [LabelIdentifier] = (WithSimplePos Expr)
data FunctionDefinition = FunctionDefinition FunctionName [WithSimplePos LabelIdentifier] (WithSimplePos Expr)

--tokensToString generated with chatgpt.
instance Show FunctionDefinition where
  show (FunctionDefinition name args body) =
    name ++ " " ++ unwords (map show args) ++ " = " ++ show body