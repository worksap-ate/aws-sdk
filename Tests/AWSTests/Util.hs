module AWSTests.Util where

import System.IO
import Test.HUnit
import Test.Hspec
import Control.Exception
import Data.Typeable
import Network.HTTP.Conduit hiding (path)
import Network.TLS (HandshakeFailed)
import Data.Maybe (isJust)

miss :: Exception e => IO a -> Selector e -> Expectation
action `miss` p = do
  r <- try action
  case r of
    Right _ -> return ()
    Left e ->
        if p e
        then do
          hPutStrLn stderr $ "exception thrown, but continue: " ++ exceptionType ++ " (" ++ show e ++ ")"
        else assertFailure $ "exception thrown: " ++ exceptionType ++ " (" ++ show e ++ ")"
  where
    -- a string repsentation of the expected exception's type
    exceptionType = (show . typeOf . instanceOf) p
      where
        instanceOf :: Selector a -> a
        instanceOf _ = error "brocken Typeable instance"

anyHttpException :: Selector SomeException
anyHttpException e
    =  isJust (fromException e :: Maybe HttpException)
    || isJust (fromException e :: Maybe HandshakeFailed)
