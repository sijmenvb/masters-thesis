module Parser.Types where

import qualified Data.Foldable as List
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
  | LetExpression [(Pattern, [LabelIdentifier], WithSimplePos Expr)] (WithSimplePos Expr)

-- used to construct a application while preserving position information
buildApplication :: WithSimplePos Expr -> WithSimplePos Expr -> WithSimplePos Expr
buildApplication expr1@(WithSimplePos start _ _) expr2@(WithSimplePos _ end _) = WithSimplePos start end $ Application expr1 expr2

expressionToArguments :: WithSimplePos Expr -> (WithSimplePos Expr, [WithSimplePos Expr])
expressionToArguments =
  let go acc exprWithPos@(WithSimplePos start end expr) =
        case expr of
          Application expr1 arg -> let (fun, args) = go (arg : acc) expr1 in (fun, args)
          _ -> (exprWithPos, acc)
   in go []

buildExpressionFromArguments :: WithSimplePos Expr -> [WithSimplePos Expr] -> WithSimplePos Expr
buildExpressionFromArguments = foldl buildApplication

-- instance Show Expr generated with chatgpt.
instance Show Expr where
  show (Parentheses expr) = "( " ++ show expr ++ " )"
  show (Application func arg) = "(" ++ show func ++ " " ++ show arg ++ " )"
  show (Int n) = show n
  show (Bool b) = show b
  show (Label label) = label
  show (LambdaAbstraction name expr) = "\\" ++ name ++ " -> " ++ show expr
  show (LetExpression bodies expr2) =
    let argumentsString arguments = concatMap (\x -> " " ++ show x) arguments
        showBody (patttern, arguments, expr1) = show patttern ++ argumentsString arguments ++ " = " ++ show expr1 ++ "\n\t"
     in "let " ++ List.concatMap showBody bodies ++ " in " ++ show expr2

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
  | TypeError String
  -- TODO: add type applications
  deriving (Eq, Ord)

-- splits a type into a list of arguments and a return type
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
  show (TypeError str) = "[Type Error : " ++ str++ "]"

type TypeVar = String

data TypeCon
  = TypeInt
  | TypeBool
  | TypeList
  | TypePair
  | TypeChar
  deriving (Eq, Ord)

instance Show TypeCon where
  show TypeInt = "Int"
  show TypeBool = "Bool"
  show TypeList = "List"
  show TypePair = "Pair"
  show TypeChar = "Char"

data Problem = Problem FunctionName String
  deriving (Show)

getNameFromProblem :: Problem -> FunctionName
getNameFromProblem (Problem name _) = name

type TypeEnvironment = Map.Map LabelIdentifier Type
