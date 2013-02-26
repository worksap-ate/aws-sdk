module AWSTests.RDSTests.EventSubscriptionTests
    ( runEventSubscriptionTests
    )
    where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Test.Hspec

import AWS.RDS
import AWS.RDS.Types (SourceType(..))
import AWSTests.Util
import AWSTests.RDSTests.Util

region :: Text
region = "ap-northeast-1"

runEventSubscriptionTests :: IO ()
runEventSubscriptionTests = hspec $ do
    describeEventSubscriptionsTest
    createDeleteModifySubscriptionTest

describeEventSubscriptionsTest :: Spec
describeEventSubscriptionsTest = do
    describe "describeEventSubscriptions doesn't fail" $ do
        it "describeEventSubscriptions doesn't throw any exception" $ do
            testRDS region (
                describeEventSubscriptions Nothing Nothing Nothing
                ) `miss` anyConnectionException

createDeleteModifySubscriptionTest :: Spec
createDeleteModifySubscriptionTest = do
    describe "{create,delete,modify}EventSubscription doesn't fail" $ do
        it "{create,delete,modify}EventSubscription doesn't throw any excpetion" $ do
            testRDS region (do
                name <- liftIO $ getRandomText "hspec-test-subscription-"
                createEventSubscription Nothing [] arn [] Nothing name
                modifyEventSubscription
                    (Just False)
                    ["creation","deletion"]
                    (Just arn)
                    (Just SourceTypeDBInstance)
                    name
                deleteEventSubscription name
                ) `miss` anyConnectionException
  where
    arn = "arn:aws:sns:ap-northeast-1:049669284607:hspec-test-topic"
