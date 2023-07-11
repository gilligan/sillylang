module Silly.Expr   where

import qualified Data.Text as T

type Identifier = T.Text

data SillyExpr = LitInt Int
               | LitString T.Text
               | LitBool Bool
               | Variable Identifier
               | BinaryOp BinOp SillyExpr SillyExpr
               deriving (Show, Eq)

data BinOp = PlusOp
           | MinusOp
           | MultiplyOp
           | EqualsOp
           | NotEqualsOp
           | LessThanOp
           | GreaterThanOp
           deriving (Show, Eq)
