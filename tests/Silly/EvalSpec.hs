module Silly.EvalSpec (spec) where

import Silly.Eval
import Silly.Expr
import Test.Hspec


spec :: Spec
spec = do
  describe "Eval" $ do
    describe "evalExpr" $ do
      it "evaluates an Integer" $ 
        evalExpr (LitInt 1) `shouldBe` IntValue 1
      it "evaluates a String" $ 
        evalExpr (LitString "text") `shouldBe` StrValue "text"
      it "evaluates a Bool" $ do
        evalExpr (LitBool True) `shouldBe` BoolValue True
        evalExpr (LitBool False) `shouldBe` BoolValue False
      it "evaluates a BinOp" $ do
        evalExpr (BinaryOp  PlusOp (LitInt 1) (LitInt 2)) `shouldBe` IntValue 3
        evalExpr (BinaryOp  MinusOp (LitInt 2) (LitInt 1)) `shouldBe` IntValue 1
        evalExpr (BinaryOp  MinusOp (LitInt 2) (LitInt 1)) `shouldBe` IntValue 1
    describe "evalBinOp" $ do
      it "evaluates 'PlusOp'" $
        evalBinOp  PlusOp (IntValue 1) (IntValue 2) `shouldBe` IntValue 3
      it "evaluates 'MinusOp'" $
        evalBinOp  MinusOp (IntValue 2) (IntValue 1) `shouldBe` IntValue 1
      it "evaluates 'EqualsOp'" $ do
        evalBinOp  EqualsOp (IntValue 1) (IntValue 1) `shouldBe` BoolValue True 
        evalBinOp  EqualsOp (IntValue 1) (IntValue 2) `shouldBe` BoolValue False 
        evalBinOp  EqualsOp (IntValue 1) (StrValue "1") `shouldBe` BoolValue False 
        evalBinOp  EqualsOp (IntValue 1) (BoolValue True) `shouldBe` BoolValue False 
        evalBinOp  EqualsOp (IntValue 1) (BoolValue False) `shouldBe` BoolValue False 

        evalBinOp  EqualsOp (StrValue "text") (StrValue "text") `shouldBe` BoolValue True 
        evalBinOp  EqualsOp (StrValue "text") (StrValue "") `shouldBe` BoolValue False 
        evalBinOp  EqualsOp (StrValue "text") (IntValue 1) `shouldBe` BoolValue False 
        evalBinOp  EqualsOp (StrValue "text") (BoolValue True) `shouldBe` BoolValue False 
        evalBinOp  EqualsOp (StrValue "text") (BoolValue False) `shouldBe` BoolValue False 

        evalBinOp  EqualsOp (BoolValue True) (BoolValue True) `shouldBe` BoolValue True 
        evalBinOp  EqualsOp (BoolValue False) (BoolValue False) `shouldBe` BoolValue True 
        evalBinOp  EqualsOp (BoolValue True) (BoolValue False) `shouldBe` BoolValue False 
        evalBinOp  EqualsOp (BoolValue True) (IntValue 1) `shouldBe` BoolValue False 
        evalBinOp  EqualsOp (BoolValue True) (StrValue "text") `shouldBe` BoolValue False 
        evalBinOp  EqualsOp (BoolValue False) (IntValue 1) `shouldBe` BoolValue False 
        evalBinOp  EqualsOp (BoolValue False) (StrValue "text") `shouldBe` BoolValue False 
      it "evaluates 'NotEqualsOp'" $ do
        evalBinOp  NotEqualsOp (IntValue 1) (IntValue 1) `shouldBe` BoolValue False 
        evalBinOp  NotEqualsOp (IntValue 1) (IntValue 2) `shouldBe` BoolValue True 
        evalBinOp  NotEqualsOp (IntValue 1) (StrValue "1") `shouldBe` BoolValue True 
        evalBinOp  NotEqualsOp (IntValue 1) (BoolValue True) `shouldBe` BoolValue True 
        evalBinOp  NotEqualsOp (IntValue 1) (BoolValue False) `shouldBe` BoolValue True 

        evalBinOp  NotEqualsOp (StrValue "text") (StrValue "text") `shouldBe` BoolValue False 
        evalBinOp  NotEqualsOp (StrValue "text") (StrValue "") `shouldBe` BoolValue True 
        evalBinOp  NotEqualsOp (StrValue "text") (IntValue 1) `shouldBe` BoolValue True 
        evalBinOp  NotEqualsOp (StrValue "text") (BoolValue True) `shouldBe` BoolValue True 
        evalBinOp  NotEqualsOp (StrValue "text") (BoolValue True) `shouldBe` BoolValue True 

        evalBinOp  NotEqualsOp (BoolValue True) (BoolValue True) `shouldBe` BoolValue False 
        evalBinOp  NotEqualsOp (BoolValue False) (BoolValue False) `shouldBe` BoolValue False 
        evalBinOp  NotEqualsOp (BoolValue True) (BoolValue True) `shouldBe` BoolValue False 
        evalBinOp  NotEqualsOp (BoolValue True) (IntValue 1) `shouldBe` BoolValue True 
        evalBinOp  NotEqualsOp (BoolValue True) (StrValue "text") `shouldBe` BoolValue True 
        evalBinOp  NotEqualsOp (BoolValue True) (IntValue 1) `shouldBe` BoolValue True 
        evalBinOp  NotEqualsOp (BoolValue True) (StrValue "text") `shouldBe` BoolValue True 
