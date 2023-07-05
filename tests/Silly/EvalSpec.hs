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
        evalBinOp  PlusOp (LitInt 1) (LitInt 2) `shouldBe` IntValue 3
      it "evaluates 'MinusOp'" $
        evalBinOp  MinusOp (LitInt 2) (LitInt 1) `shouldBe` IntValue 1
      it "evaluates 'EqualsOp'" $ do
        evalBinOp  EqualsOp (LitInt 1) (LitInt 1) `shouldBe` BoolValue True 
        evalBinOp  EqualsOp (LitInt 1) (LitInt 2) `shouldBe` BoolValue False 
        evalBinOp  EqualsOp (LitInt 1) (LitString "1") `shouldBe` BoolValue False 
        evalBinOp  EqualsOp (LitInt 1) (LitBool True) `shouldBe` BoolValue False 
        evalBinOp  EqualsOp (LitInt 1) (LitBool False) `shouldBe` BoolValue False 

        evalBinOp  EqualsOp (LitString "text") (LitString "text") `shouldBe` BoolValue True 
        evalBinOp  EqualsOp (LitString "text") (LitString "") `shouldBe` BoolValue False 
        evalBinOp  EqualsOp (LitString "text") (LitInt 1) `shouldBe` BoolValue False 
        evalBinOp  EqualsOp (LitString "text") (LitBool True) `shouldBe` BoolValue False 
        evalBinOp  EqualsOp (LitString "text") (LitBool False) `shouldBe` BoolValue False 

        evalBinOp  EqualsOp (LitBool True) (LitBool True) `shouldBe` BoolValue True 
        evalBinOp  EqualsOp (LitBool False) (LitBool False) `shouldBe` BoolValue True 
        evalBinOp  EqualsOp (LitBool True) (LitBool False) `shouldBe` BoolValue False 
        evalBinOp  EqualsOp (LitBool True) (LitInt 1) `shouldBe` BoolValue False 
        evalBinOp  EqualsOp (LitBool True) (LitString "text") `shouldBe` BoolValue False 
        evalBinOp  EqualsOp (LitBool False) (LitInt 1) `shouldBe` BoolValue False 
        evalBinOp  EqualsOp (LitBool False) (LitString "text") `shouldBe` BoolValue False 
      it "evaluates 'NotEqualsOp'" $ do
        evalBinOp  NotEqualsOp (LitInt 1) (LitInt 1) `shouldBe` BoolValue False 
        evalBinOp  NotEqualsOp (LitInt 1) (LitInt 2) `shouldBe` BoolValue True 
        evalBinOp  NotEqualsOp (LitInt 1) (LitString "1") `shouldBe` BoolValue True 
        evalBinOp  NotEqualsOp (LitInt 1) (LitBool True) `shouldBe` BoolValue True 
        evalBinOp  NotEqualsOp (LitInt 1) (LitBool False) `shouldBe` BoolValue True 

        evalBinOp  NotEqualsOp (LitString "text") (LitString "text") `shouldBe` BoolValue False 
        evalBinOp  NotEqualsOp (LitString "text") (LitString "") `shouldBe` BoolValue True 
        evalBinOp  NotEqualsOp (LitString "text") (LitInt 1) `shouldBe` BoolValue True 
        evalBinOp  NotEqualsOp (LitString "text") (LitBool True) `shouldBe` BoolValue True 
        evalBinOp  NotEqualsOp (LitString "text") (LitBool True) `shouldBe` BoolValue True 

        evalBinOp  NotEqualsOp (LitBool True) (LitBool True) `shouldBe` BoolValue False 
        evalBinOp  NotEqualsOp (LitBool False) (LitBool False) `shouldBe` BoolValue False 
        evalBinOp  NotEqualsOp (LitBool True) (LitBool True) `shouldBe` BoolValue False 
        evalBinOp  NotEqualsOp (LitBool True) (LitInt 1) `shouldBe` BoolValue True 
        evalBinOp  NotEqualsOp (LitBool True) (LitString "text") `shouldBe` BoolValue True 
        evalBinOp  NotEqualsOp (LitBool True) (LitInt 1) `shouldBe` BoolValue True 
        evalBinOp  NotEqualsOp (LitBool True) (LitString "text") `shouldBe` BoolValue True 
