module AWSTests.RDSTests.EventSubscriptionTests
    ( runEventSubscriptionTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.RDS
import AWSTests.Util
import AWSTests.RDSTests.Util

region :: Text
region = "ap-northeast-1"

runEventSubscriptionTests :: IO ()
runEventSubscriptionTests = hspec $ do
    describeEventSubscriptionsTest

describeEventSubscriptionsTest :: Spec
describeEventSubscriptionsTest = do
    describe "describeEventSubscriptions doesn't fail" $ do
        it "describeEventSubscriptions doesn't throw any exception" $ do
            testRDS region (
                describeEventSubscriptions Nothing Nothing Nothing
                ) `miss` anyConnectionException
