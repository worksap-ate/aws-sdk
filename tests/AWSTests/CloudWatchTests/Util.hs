{-# LANGUAGE CPP #-}

module AWSTests.CloudWatchTests.Util
    ( testCloudWatch
    )
    where

import Data.Text (Text)
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
#endif

import AWS
import AWS.CloudWatch

testCloudWatch
    :: Text
    -> CloudWatch (ResourceT IO) a
    -> IO a
testCloudWatch region request = do
    cred <- loadCredential
    runResourceT $ do
        runCloudWatch cred $ do
            setRegion region
            request
