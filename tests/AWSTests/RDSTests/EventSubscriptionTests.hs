module AWSTests.RDSTests.EventSubscriptionTests
    ( runEventSubscriptionTests
    )
    where

import Control.Monad.IO.Class (liftIO)
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
    createAndDeleteSubscriptionTest

describeEventSubscriptionsTest :: Spec
describeEventSubscriptionsTest = do
    describe "describeEventSubscriptions doesn't fail" $ do
        it "describeEventSubscriptions doesn't throw any exception" $ do
            testRDS region (
                describeEventSubscriptions Nothing Nothing Nothing
                ) `miss` anyConnectionException

createAndDeleteSubscriptionTest :: Spec
createAndDeleteSubscriptionTest = do
    describe "{create,delete}EventSubscription doesn't fail" $ do
        it "{create,delete}EventSubscription doesn't throw any excpetion" $ do
            testRDS region (do
                name <- liftIO $ getRandomText "hspec-test-subscription-"
                createEventSubscription Nothing [] arn [] Nothing name
                deleteEventSubscription name
                ) `miss` anyConnectionException
  where
    arn = "arn:aws:sns:ap-northeast-1:049669284607:hspec-test-topic"
