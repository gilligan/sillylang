module Silly.EvalSpec (spec) where

import Silly.Eval
import Silly.Expr
import Test.Hspec

infix 1 `shouldEvalTo`

shouldEvalTo :: SillyExpr -> Either SillyException SillyVal -> Expectation
shouldEvalTo expression expected = do
  x <- runEvalExpr expression
  x `shouldBe` expected


spec :: Spec
spec = do
  describe "Eval" $ do
    describe "evalExpr'" $ do
      it "evaluates an Integer" $ do
        LitInt 1 `shouldEvalTo` Right (IntValue 1)
      it "evaluates a String" $
        LitString "text" `shouldEvalTo` Right (StrValue "text")
      it "evaluates a Bool" $ do
        LitBool True `shouldEvalTo` Right (BoolValue True)
        LitBool False `shouldEvalTo` Right (BoolValue False)
      it "evaluates a BinOp" $ do
        BinaryOp PlusOp (LitInt 1) (LitInt 2) `shouldEvalTo` Right (IntValue 3)
        BinaryOp MinusOp (LitInt 2) (LitInt 1) `shouldEvalTo` Right (IntValue 1)
        BinaryOp MinusOp (LitInt 2) (LitInt 1) `shouldEvalTo` Right (IntValue 1)
      it "evaluates a variable reference" $ do
        Variable "meaning" `shouldEvalTo` Right (IntValue 42)
    describe "evalBinOp" $ do
      it "successfully evaluates 'PlusOp' on integers" $
        BinaryOp PlusOp (LitInt 1) (LitInt 2) `shouldEvalTo` Right (IntValue 3)
      it "fails to evaluate invalid 'PlusOp' on non-integer arguments" $ do
        BinaryOp PlusOp (LitInt 1) (LitBool True) `shouldEvalTo` Left (Exception "evaluation error: Addition on non-integer arguments")
        BinaryOp PlusOp (LitInt 1) (LitString "1") `shouldEvalTo` Left (Exception "evaluation error: Addition on non-integer arguments")
      it "successfully evaluates 'MinusOp' on integers" $
        BinaryOp MinusOp (LitInt 2) (LitInt 1) `shouldEvalTo` Right (IntValue 1)
      it "fails to evaluate invalid 'MinusOp' on non-integer arguments" $ do
        BinaryOp MinusOp (LitInt 2) (LitBool True) `shouldEvalTo` Left (Exception "evaluation error: Subtraction on non-integer arguments")
        BinaryOp MinusOp (LitInt 2) (LitString "1") `shouldEvalTo` Left (Exception "evaluation error: Subtraction on non-integer arguments")
      it "successfully evaluates 'MultiplyOp' on integers" $ do
        BinaryOp  MultiplyOp (LitInt 2) (LitInt 2) `shouldEvalTo` Right (IntValue 4)
      it "fails to evaluate invalid 'MultiplyOp' on non-integer arguments" $ do
        BinaryOp MultiplyOp (LitInt 2) (LitBool True) `shouldEvalTo` Left (Exception "evaluation error: Multiplication on non-integer arguments")
        BinaryOp MultiplyOp (LitInt 2) (LitString "1") `shouldEvalTo` Left (Exception "evaluation error: Multiplication on non-integer arguments")
      it "evaluates 'EqualsOp'" $ do
        BinaryOp EqualsOp (LitInt 1) (LitInt 1) `shouldEvalTo` Right (BoolValue True)
        BinaryOp EqualsOp (LitInt 1) (LitInt 2) `shouldEvalTo` Right (BoolValue False)
        BinaryOp EqualsOp (LitInt 1) (LitString "1") `shouldEvalTo` Right (BoolValue False)
        BinaryOp EqualsOp (LitInt 1) (LitBool True) `shouldEvalTo` Right (BoolValue False)
        BinaryOp EqualsOp (LitInt 1) (LitBool False) `shouldEvalTo` Right (BoolValue False)

        BinaryOp EqualsOp (LitString "text") (LitString "text") `shouldEvalTo` Right (BoolValue True)
        BinaryOp EqualsOp (LitString "text") (LitString "") `shouldEvalTo` Right (BoolValue False)
        BinaryOp EqualsOp (LitString "text") (LitInt 1) `shouldEvalTo` Right (BoolValue False)
        BinaryOp EqualsOp (LitString "text") (LitBool True) `shouldEvalTo` Right (BoolValue False)
        BinaryOp EqualsOp (LitString "text") (LitBool False) `shouldEvalTo` Right (BoolValue False)

        BinaryOp EqualsOp (LitBool True) (LitBool True) `shouldEvalTo` Right (BoolValue True)
        BinaryOp EqualsOp (LitBool False) (LitBool False) `shouldEvalTo` Right (BoolValue True)
        BinaryOp EqualsOp (LitBool True) (LitBool False) `shouldEvalTo` Right (BoolValue False)
        BinaryOp EqualsOp (LitBool True) (LitInt 1) `shouldEvalTo` Right (BoolValue False)
        BinaryOp EqualsOp (LitBool True) (LitString "text") `shouldEvalTo` Right (BoolValue False)
        BinaryOp EqualsOp (LitBool False) (LitInt 1) `shouldEvalTo` Right (BoolValue False)
        BinaryOp EqualsOp (LitBool False) (LitString "text") `shouldEvalTo` Right (BoolValue False)
      it "evaluates 'NotEqualsOp'" $ do
        BinaryOp NotEqualsOp (LitInt 1) (LitInt 1) `shouldEvalTo` Right (BoolValue False)
        BinaryOp NotEqualsOp (LitInt 1) (LitInt 2) `shouldEvalTo` Right (BoolValue True)
        BinaryOp NotEqualsOp (LitInt 1) (LitString "1") `shouldEvalTo` Right (BoolValue True)
        BinaryOp NotEqualsOp (LitInt 1) (LitBool True) `shouldEvalTo` Right (BoolValue True)
        BinaryOp NotEqualsOp (LitInt 1) (LitBool False) `shouldEvalTo` Right (BoolValue True)

        BinaryOp NotEqualsOp (LitString "text") (LitString "text") `shouldEvalTo` Right (BoolValue False)
        BinaryOp NotEqualsOp (LitString "text") (LitString "") `shouldEvalTo` Right (BoolValue True)
        BinaryOp NotEqualsOp (LitString "text") (LitInt 1) `shouldEvalTo` Right (BoolValue True)
        BinaryOp NotEqualsOp (LitString "text") (LitBool True) `shouldEvalTo` Right (BoolValue True)
        BinaryOp NotEqualsOp (LitString "text") (LitBool True) `shouldEvalTo` Right (BoolValue True)

        BinaryOp NotEqualsOp (LitBool True) (LitBool True) `shouldEvalTo` Right (BoolValue False)
        BinaryOp NotEqualsOp (LitBool False) (LitBool False) `shouldEvalTo` Right (BoolValue False)
        BinaryOp NotEqualsOp (LitBool True) (LitBool True) `shouldEvalTo` Right (BoolValue False)
        BinaryOp NotEqualsOp (LitBool True) (LitInt 1) `shouldEvalTo` Right (BoolValue True)
        BinaryOp NotEqualsOp (LitBool True) (LitString "text") `shouldEvalTo` Right (BoolValue True)
        BinaryOp NotEqualsOp (LitBool True) (LitInt 1) `shouldEvalTo` Right (BoolValue True)
        BinaryOp NotEqualsOp (LitBool True) (LitString "text") `shouldEvalTo` Right (BoolValue True)
    describe "SillyInterpreter" $ do
      it "can lookup pre-defined variables" $ do
        res <- runWithInitialState $
          lookupVar "meaning"
        res `shouldBe` Right (IntValue 42)
      it "can lookup defined variables" $ do
        res <- runWithInitialState $ do
         runStmt $ VarStmt "foobar" (LitInt 1337) 
         lookupRef "foobar"
        res `shouldBe` Right (Just $ IntValue 1337)
