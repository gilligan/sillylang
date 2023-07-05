module Silly.Expr   where

import qualified Data.Text as T

data SillyExpr = LitInt Int
               | LitString T.Text
               | LitBool Bool
               | BinaryOp BinOp SillyExpr SillyExpr
               deriving (Show, Eq)

data BinOp = PlusOp
           | MinusOp
           | EqualsOp
           | NotEqualsOp
           | LessThanOp
           | GreaterThanOp
           deriving (Show, Eq)
