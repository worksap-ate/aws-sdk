module PropExample
    ( propSpecs
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

propSpecs :: Spec
propSpecs = do
    describe "These are some simple prop tests 1" $ do
        it "should be original list if apply reverse twice" $ property prop_rev
        it "should be greater than or equal to args" $ property prop_plus
    describe "These are some simple prop tests 2" $ do
        prop "should be original list if apply reverse twice" $ prop_rev
        prop "should be greater than or equal to args" $ prop_plus

prop_rev :: [Int] -> Bool
prop_rev xs = xs == xs'
  where
    xs' = reverse . reverse $ xs

prop_plus :: Integer -> Integer -> Property
prop_plus a b =
    a >= 0 && b >= 0
        ==> c >= a .&&. c >= b
  where
    c = a + b
