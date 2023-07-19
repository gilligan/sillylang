{-# LANGUAGE LambdaCase #-}

module Silly.Eval where

import Control.Monad (when)
import Control.Monad.Base (MonadBase)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT)
import Control.Monad.State.Strict qualified as State
import Data.Foldable (traverse_)
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

class (Functor m, Applicative m, Monad m, MonadIO m, MonadBase IO m, MonadState InterpreterState m, MonadError SillyException m) => SillyInterpreter m where
  lookupRef :: Identifier -> m (Maybe SillyVal)
  runStmt :: Stmt -> m ()

instance SillyInterpreter Interpreter where
  lookupRef r = do
    env <- State.gets sillyEnv
    case Map.lookup r env of
      Just ref -> do
        val <- readIORef ref
        return $ Just val
      Nothing -> return Nothing
  runStmt = runStatement

runWithInitialState :: Interpreter a -> IO (Either SillyException a)
runWithInitialState a = do
  s <- initialState
  flip evalStateT s . runExceptT . runInterpreter $ a

-- | Runs an expression evaluation through the Interpreter monad stack
runEvalExpr :: SillyExpr -> IO (Either SillyException SillyVal)
runEvalExpr exp = do
  s <- initialState
  flip evalStateT s . runExceptT . runInterpreter $ evalExpr exp

-- | Throws an Exception in the Interpreter monad stack with the given description
runtimeError :: SillyInterpreter m => T.Text -> m a
runtimeError = throwError . Exception

-- | Evaluates an expression
evalExpr :: SillyInterpreter m => SillyExpr -> m SillyVal
evalExpr = \case
  LitInt x -> return $ IntValue x
  LitString str -> return $ StrValue str
  LitBool x -> return $ BoolValue x
  BinaryOp op l r -> evalBinOp op l r
  Variable varId -> lookupVar varId

-- | Retrieves a variable specified by identifier.
-- An runtime exception will be thrown if the identifier is not present in the runtime environment
lookupVar :: SillyInterpreter m => Identifier -> m SillyVal
lookupVar varId = State.gets sillyEnv >>= getRef varId >>= readIORef

-- | Obtain a reference from the environment
getRef :: SillyInterpreter m => Identifier -> Env -> m (IORef SillyVal)
getRef idVar env =
  case Map.lookup idVar env of
    Just ref -> return ref
    Nothing -> runtimeError "undefined variable"

isTruthy :: SillyVal -> Bool
isTruthy = \case
  IntValue num -> (num /= 0)
  StrValue str -> (str /= "")
  BoolValue bool -> bool

-- | Execute a statement in the interpreter
runStatement :: SillyInterpreter m => Stmt -> m ()
runStatement = \case
  ExprStmt expr -> State.void $ evalExpr expr
  VarStmt varId expr -> evalExpr expr >>= defineVar varId
  IfCond cond stmt -> do
    c <- evalExpr cond
    when (isTruthy c) $ traverse_ runStatement stmt

-- | Update the Interpreter State environment
updateEnv :: SillyInterpreter m => Env -> m ()
updateEnv env = State.modify' $ \s -> s {sillyEnv = env}

-- | Execute a variable declaration by name and value
defineVar :: SillyInterpreter m => Identifier -> SillyVal -> m ()
defineVar varId val = do
  env <- State.gets sillyEnv
  if Map.member varId env
    then runtimeError $ "variable '" <> varId <> "' has already been defined"
    else newIORef val >>= \ref -> updateEnv $ Map.insert varId ref env

-- | Evaluate a binary operation expression
evalBinOp :: SillyInterpreter m => BinOp -> SillyExpr -> SillyExpr -> m SillyVal
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
