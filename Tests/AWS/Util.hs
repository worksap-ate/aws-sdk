module Tests.AWS.Util where

import Test.Hspec
import Control.Exception

shouldntThrow :: Exception e => IO a -> Selector e -> Expectation
action `shouldntThrow` p = do
  action `shouldThrow` (\e -> if p e then False else True)
