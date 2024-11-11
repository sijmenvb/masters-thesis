module Parser.Types where

import qualified Data.Map as Map
import Data.Maybe
import Parser.ParserBase

type FunctionName = String

type LabelIdentifier = String

type Pattern = String

data Expr
  = Parentheses (WithSimplePos Expr)
  | Application (WithSimplePos Expr) (WithSimplePos Expr)
  | Int Int
  | Bool Bool
  | Label LabelIdentifier
  | LambdaAbstraction LabelIdentifier (WithSimplePos Expr)
  | LetExpression Pattern (WithSimplePos Expr) (WithSimplePos Expr)

-- used to construct a application while preserving position information
buildApplication :: WithSimplePos Expr -> WithSimplePos Expr -> WithSimplePos Expr
buildApplication expr1@(WithSimplePos start _ _) expr2@(WithSimplePos _ end _) = WithSimplePos start end $ Application expr1 expr2

-- instance Show Expr generated with chatgpt.
instance Show Expr where
  show (Parentheses expr) = "( " ++ show expr ++ " )"
  show (Application func arg) = "(" ++ show func ++ " " ++ show arg ++ " )"
  show (Int n) = show n
  show (Bool b) = show b
  show (Label label) = label
  show (LambdaAbstraction name expr) = "\\" ++ name ++ " -> " ++ show expr
  show (LetExpression patttern expr1 expr2) = "let " ++ show patttern ++ " = " ++ show expr1 ++ " in " ++ show expr2

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
    name ++ " :: " ++ show typ

data Type
  = TypeVar TypeVar
  | FreshVar Int
  | TypeCon TypeCon
  | TypeArrow Type Type
  -- TODO: add type applications
  deriving (Eq, Ord)

typeToArguments :: Type -> ([Type], Type)
typeToArguments typ =
  case typ of
    (TypeArrow arg rest) ->
      let (args, result) = typeToArguments rest
       in (arg : args, result)
    _ -> ([], typ)

buildTypeFromArguments :: [Type] -> Type -> Type
buildTypeFromArguments arguments returnType = foldr TypeArrow returnType arguments

instance Show Type where
  show (TypeVar typeVar) = typeVar
  show (FreshVar num) = "v" ++ show num
  show (TypeCon typeCon) = show typeCon
  show (TypeArrow typ1 typ2) = "(" ++ show typ1 ++ " -> " ++ show typ2 ++ ")"

type TypeVar = String

data TypeCon
  = TypeInt
  | TypeBool
  | TypeList
  | TypePair
  deriving (Eq, Ord)

instance Show TypeCon where
  show TypeInt = "Int"
  show TypeBool = "Bool"
  show TypeList = "List"
  show TypePair = "Pair"

data Problem = Problem FunctionName String
  deriving (Show)

getNameFromProblem :: Problem -> FunctionName
getNameFromProblem (Problem name _) = name

type TypeEnvironment = Map.Map LabelIdentifier Type
