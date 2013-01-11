module AWSTests.RDSTests.Util
    ( testRDS
    )
    where

import Data.Text (Text)
import Data.Conduit (ResourceT, runResourceT)

import AWS
import AWS.RDS

testRDS
    :: Text
    -> RDS (ResourceT IO) a
    -> IO a
testRDS region request = do
    cred <- loadCredential
    runResourceT $ do
        runRDS cred $ do
            setRegion region
            request
