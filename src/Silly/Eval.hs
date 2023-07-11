{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Silly.Eval where

import Control.Monad.Except (ExceptT, MonadError (..),runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT)
import Data.IORef.Lifted
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Silly.Expr

newtype SillyException = Exception T.Text
  deriving (Eq, Show)

data SillyVal
  = IntValue Int
  | StrValue T.Text
  | BoolValue Bool
  deriving (Show, Eq)

-- The result of an evaluation is either an error text or
-- a SillyVal
type EvalResult = Either T.Text SillyVal

type Env = Map.Map Identifier (IORef SillyVal)

initialEnv' :: IO Env
initialEnv' = do
  meaning <- newIORef (IntValue 42)
  return $
    Map.fromList
      [ ("meaning", meaning)
      ]

newtype InterpreterState = InterpreterState {interpEnv :: Env}

newtype Interpreter a = Interpreter
  { runInterpreter :: ExceptT SillyException (StateT InterpreterState IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState InterpreterState,
      MonadError SillyException
    )

runEvalExpr :: SillyExpr -> IO (Either SillyException SillyVal)
runEvalExpr exp = flip evalStateT s . runExceptT . runInterpreter $ evalExpr exp
  where
    s = InterpreterState  Map.empty
    
runtimeError :: T.Text -> Interpreter a
runtimeError = throwError . Exception

evalExpr :: SillyExpr -> Interpreter SillyVal
evalExpr = \case
  LitInt x -> return $ IntValue x
  LitString str -> return $ StrValue str
  LitBool x -> return $ BoolValue x
  BinaryOp op l r -> evalBinOp op l r
  Variable _ -> undefined

evalBinOp :: BinOp -> SillyExpr -> SillyExpr -> Interpreter SillyVal
evalBinOp op l r = do
  leftVal <- evalExpr l
  rightVal <- evalExpr r
  case (op, leftVal, rightVal) of
    (PlusOp, IntValue x, IntValue y) -> return $ IntValue $ x + y
    (PlusOp, _, _) -> runtimeError "evaluation error: Addition on non-integer arguments"
    -- Subtraction
    (MinusOp, IntValue x, IntValue y) -> return $ IntValue $ x - y
    (MinusOp, _, _) -> runtimeError "evaluation error: Subtraction on non-integer arguments"
    -- Multiplication
    (MultiplyOp, IntValue x, IntValue y) -> return $ IntValue $ x * y
    (MultiplyOp, _, _) -> runtimeError "evaluation error: Multiplication on non-integer arguments"
    -- Equality
    (EqualsOp, IntValue x, IntValue y) -> return $ BoolValue $ x == y
    (EqualsOp, IntValue _, _) -> return $ BoolValue False
    (EqualsOp, StrValue x, StrValue y) -> return $ BoolValue $ x == y
    (EqualsOp, StrValue _, _) -> return $ BoolValue False
    (EqualsOp, BoolValue x, BoolValue y) -> return $ BoolValue $ x == y
    (EqualsOp, BoolValue _, _) -> return $ BoolValue False
    -- Non-Equality
    (NotEqualsOp, IntValue x, IntValue y) -> return $ BoolValue $ x /= y
    (NotEqualsOp, IntValue _, _) -> return $ BoolValue True
    (NotEqualsOp, StrValue x, StrValue y) -> return $ BoolValue $ x /= y
    (NotEqualsOp, StrValue _, _) -> return $ BoolValue True
    (NotEqualsOp, BoolValue x, BoolValue y) -> return $ BoolValue $ x /= y
    (NotEqualsOp, BoolValue _, _) -> return $ BoolValue True
    -- Less-Than
    (LessThanOp, IntValue x, IntValue y) -> return $ BoolValue $ x < y
    (LessThanOp, _, _) -> runtimeError "evaluation error: less-than comparison on non-integer arguments"
    -- Greater-Than
    (GreaterThanOp, IntValue x, IntValue y) -> return $ BoolValue $ x < y
    (GreaterThanOp, _, _) -> runtimeError "evaluation error: greater-than comparison on non-integer arguments"
