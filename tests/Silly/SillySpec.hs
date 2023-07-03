module Silly.SillySpec (spec) where

import Silly.Silly
import Test.Hspec


spec :: Spec
spec = do
  describe "Silly" $ do
    describe "Silly" $ do
      it "is silly" $ do
        silly `shouldBe` "silly"
