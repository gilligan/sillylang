module Silly.Expr where

import Data.Text qualified as T

type Identifier = T.Text

-- | Expressions supported by SillyLang
data SillyExpr
  = -- | A number
    LitInt Int
  | -- | A String
    LitString T.Text
  | -- | A Boolean
    LitBool Bool
  | -- | A variable
    Variable Identifier
  | -- | A binary operation
    BinaryOp BinOp SillyExpr SillyExpr
  deriving (Show, Eq)

-- | A binary operation
data BinOp
  = -- | Addition
    PlusOp
  | -- | Subtraction
    MinusOp
  | -- | Multiplication
    MultiplyOp
  | -- | Equality
    EqualsOp
  | -- | Non-Equality
    NotEqualsOp
  | -- | Less-Than
    LessThanOp
  | -- | Greater-Than
    GreaterThanOp
  deriving (Show, Eq)

-- | A SillyLang Statement
data Stmt
  = -- | An expression
    ExprStmt SillyExpr
  | -- | Variable declaration
    VarStmt Identifier SillyExpr
  | -- | If Clause
    IfCond SillyExpr [Stmt]

type Program = [Stmt]
