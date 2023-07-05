module Silly.Eval where

import Data.Text qualified as T
import Silly.Expr

data SillyVal
  = IntValue Int
  | StrValue T.Text
  | BoolValue Bool
  deriving (Show, Eq)

evalExpr :: SillyExpr -> SillyVal
evalExpr (LitInt x) = IntValue x
evalExpr (LitString x) = StrValue x
evalExpr (LitBool x) = BoolValue x
evalExpr (BinaryOp op a b) = evalBinOp op a b

evalBinOp :: BinOp -> SillyExpr -> SillyExpr -> SillyVal
evalBinOp op a b =
  let leftVal = evalExpr a
      rightVal = evalExpr b
   in case (op, leftVal, rightVal) of
        (PlusOp, IntValue x, IntValue y) -> IntValue $ x + y
        (PlusOp, _, _) -> error "invalid expression"
        (MinusOp, IntValue x, IntValue y) -> IntValue $ x - y
        (MinusOp, _, _) -> error "invalid expression"
        (EqualsOp, IntValue x, IntValue y) -> BoolValue $ x == y
        (EqualsOp, IntValue _, _) -> BoolValue False
        (EqualsOp, StrValue x, StrValue y) -> BoolValue $ x == y
        (EqualsOp, StrValue _, _) -> BoolValue False
        (EqualsOp, BoolValue x, BoolValue y) -> BoolValue $ x == y
        (EqualsOp, BoolValue _, _) -> BoolValue False
        (NotEqualsOp, IntValue x, IntValue y) -> BoolValue $ x /= y
        (NotEqualsOp, IntValue _, _) -> BoolValue True
        (NotEqualsOp, StrValue x, StrValue y) -> BoolValue $ x /= y
        (NotEqualsOp, StrValue _, _) -> BoolValue True
        (NotEqualsOp, BoolValue x, BoolValue y) -> BoolValue $ x /= y
        (NotEqualsOp, BoolValue _, _) -> BoolValue True
        (LessThanOp, IntValue x, IntValue y) -> BoolValue $ x < y
        (LessThanOp, _, _) -> error "invalid expression"
        (GreaterThanOp, IntValue x, IntValue y) -> BoolValue $ x < y
        (GreaterThanOp, _, _) -> error "invalid expression"
