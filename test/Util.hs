module Util where

import System.IO
import System.Random
import Test.HUnit
import Test.Hspec
import Control.Exception
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Cloud.AWS (AWSException(..))

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

anyConnectionException :: Selector AWSException
anyConnectionException (ConnectionException _) = True
anyConnectionException _ = False

getRandomText :: Text -> IO Text
getRandomText t = do
    str <- sequence $ replicate 10 $ randomRIO ('a', 'z')
    return $ t <> T.pack str
