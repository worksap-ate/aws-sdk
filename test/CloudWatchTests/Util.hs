module CloudWatchTests.Util
    ( testCloudWatch
    )
    where

import Control.Monad.Trans.Resource(ResourceT, runResourceT)
import Data.Text (Text)

import Cloud.AWS.CloudWatch

testCloudWatch
    :: Text
    -> CloudWatch (ResourceT IO) a
    -> IO a
testCloudWatch region request = do
    runResourceT $ runCloudWatch $ do
        setRegion region
        request
