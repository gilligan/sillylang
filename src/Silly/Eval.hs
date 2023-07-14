{-# LANGUAGE LambdaCase #-}

module Silly.Eval where

import Control.Monad.Base (MonadBase)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT)
import Control.Monad.State.Strict qualified as State
import Data.IORef.Lifted
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Silly.Expr

newtype SillyException = Exception T.Text
  deriving (Eq, Show)

-- | Runtime values that have been evaluated from Expressions
data SillyVal
  = -- | a number value
    IntValue Int
  | -- | a string value
    StrValue T.Text
  | -- | a boolean value
    BoolValue Bool
  deriving (Show, Eq)

-- | Evaluation environment.
-- The evaluation environment will eventually store variables or
-- functions during runtime.
type Env = Map.Map Identifier (IORef SillyVal)

-- | Creates an initial environment.
-- The initial environment provides builtin functions and variable declarations
initialState :: IO InterpreterState
initialState = do
  meaning <- newIORef (IntValue 42)
  return $
    InterpreterState $
      Map.fromList
        [ ("meaning", meaning)
        ]

newtype InterpreterState = InterpreterState {sillyEnv :: Env}

newtype Interpreter a = Interpreter
  { runInterpreter :: ExceptT SillyException (StateT InterpreterState IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadBase IO,
      MonadState InterpreterState,
      MonadError SillyException
    )

-- | Runs an expression evaluation through the Interpreter monad stack
runEvalExpr :: SillyExpr -> IO (Either SillyException SillyVal)
runEvalExpr exp = do
  s <- initialState
  flip evalStateT s . runExceptT . runInterpreter $ evalExpr exp

-- | Throws an Exception in the Interpreter monad stack with the given description
runtimeError :: T.Text -> Interpreter a
runtimeError = throwError . Exception

-- | Evaluates an expression
evalExpr :: SillyExpr -> Interpreter SillyVal
evalExpr = \case
  LitInt x -> return $ IntValue x
  LitString str -> return $ StrValue str
  LitBool x -> return $ BoolValue x
  BinaryOp op l r -> evalBinOp op l r
  Variable varId -> lookupVar varId

-- | Retrieves a variable specified by identifier.
-- An runtime exception will be thrown if the identifier is not present in the runtime environment
lookupVar :: Identifier -> Interpreter SillyVal
lookupVar varId = State.gets sillyEnv >>= getRef varId >>= readIORef

-- | Obtain a reference from the environment
getRef :: Identifier -> Env -> Interpreter (IORef SillyVal)
getRef idVar env =
  case Map.lookup idVar env of
    Just ref -> return ref
    Nothing -> runtimeError "undefined variable"

-- | Execute a statement in the interpreter
runStatement :: Stmt -> Interpreter ()
runStatement = \case
  ExprStmt expr -> State.void $ evalExpr expr
  VarStmt varId expr -> evalExpr expr >>= defineVar varId

-- | Update the Interpreter State environment
updateEnv :: Env -> Interpreter ()
updateEnv env = State.modify' $ \s -> s {sillyEnv = env}

-- | Execute a variable declaration by name and value
defineVar :: Identifier -> SillyVal -> Interpreter ()
defineVar varId val = do
  env <- State.gets sillyEnv
  if Map.member varId env
    then runtimeError $ "variable '" <> varId <> "' has already been defined"
    else newIORef val >>= \ref -> updateEnv $ Map.insert varId ref env

-- | Evaluate a binary operation expression
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
