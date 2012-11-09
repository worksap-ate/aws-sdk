module SpecExample
    ( normalSpecs
    ) where

import Test.Hspec
import Control.Exception

normalSpecs :: Spec
normalSpecs = do
    describe "These are some simple tests 1" $ do
        it "should be 2 if 1 + 1" $ do
            1 + 1 `shouldBe` 2
        it "should be 0 if 1 - 1" $ do
            1 - 1 `shouldBe` 0
        it "should be even if 3 + 5" $ do
            3 + 5 `shouldSatisfy` even
        it "should be thrown some Exception if 0 `div` 0 evaluated" $ do
            print (0 `div` 0) `shouldThrow` anyException
        it "should be thrown ArithException if 0 `div` 0 evaluated" $ do
            print (0 `div` 0) `shouldThrow` anyArithException
        it "should be thrown DivideByZero if 0 `div` 0 evaluated" $ do
            print (0 `div` 0) `shouldThrow` (== DivideByZero)
