module AWSTests.CloudWatchTests.Util
    ( testCloudWatch
    )
    where

import Data.Text (Text)
import Data.Conduit (ResourceT, runResourceT)

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
