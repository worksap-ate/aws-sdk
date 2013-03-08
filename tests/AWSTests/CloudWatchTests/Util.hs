module AWSTests.CloudWatchTests.Util
    ( testCloudWatch
    )
    where

import Data.Text (Text)
import Data.Conduit (ResourceT, runResourceT)

import Cloud.AWS.CloudWatch

testCloudWatch
    :: Text
    -> CloudWatch (ResourceT IO) a
    -> IO a
testCloudWatch region request = do
    runResourceT $ runCloudWatch $ do
        setRegion region
        request
