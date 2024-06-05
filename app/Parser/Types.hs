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

-- instance Show Expr generated with chatgpt.
instance Show Expr where
  show (Parentheses expr) = "( " ++ show expr ++ " )"
  show (Application func arg) = show func ++ " " ++ show arg
  show (Int n) = show n
  show (Bool b) = show b
  show (Label label) = label

-- used to store function definitions that look like:
-- FunctionName [LabelIdentifier] = (WithSimplePos Expr)
data Section
  = FunctionDefinition FunctionName [WithSimplePos LabelIdentifier] (WithSimplePos Expr)
  | FunctionType FunctionName (WithSimplePos Type)

-- tokensToString generated with chatgpt.
instance Show Section where
  show (FunctionDefinition name args body) =
    name ++ " " ++ unwords (map show args) ++ " = " ++ show body
  show (FunctionType name typ) =
    name ++  " :: " ++ show typ

data Type
  = TypeVar TypeVar
  | TypeCon TypeCon
  | TypeArrow Type Type

instance Show Type where
    show (TypeVar typeVar) = typeVar
    show (TypeCon typeCon) = show typeCon
    show (TypeArrow typ1 typ2) = show typ1 ++ " -> " ++ show typ2
    

type TypeVar = String

data TypeCon
  = TypeInt
  | TypeBool
  | TypeFun
  | TypeList
  | TypePair

instance Show TypeCon where
  show TypeInt = "Int"
  show TypeBool = "Bool"
  show TypeFun = "func"
  show TypeList = "List"
  show TypePair = "Pair"