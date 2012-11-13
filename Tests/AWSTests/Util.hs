module AWSTests.Util where

import Test.HUnit
import Test.Hspec
import Control.Exception
import Data.Typeable

shouldntThrow :: Exception e => IO a -> Selector e -> Expectation
action `shouldntThrow` p = do
  r <- try action
  case r of
    Right _ -> return ()
    Left e ->
      (`assertBool` (not (p e))) $
        "exception thrown: " ++ exceptionType ++ " (" ++ show e ++ ")"
  where
    -- a string repsentation of the expected exception's type
    exceptionType = (show . typeOf . instanceOf) p
      where
        instanceOf :: Selector a -> a
        instanceOf _ = error "brocken Typeable instance"
