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
    createAndDeleteSubscriptionTest
    modifySubscriptionTest

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
                withEventSubscription name snsTopicArn $
                    const $ return ()
                ) `miss` anyConnectionException

snsTopicArn :: Text
snsTopicArn = "arn:aws:sns:ap-northeast-1:049669284607:hspec-test-topic"

modifySubscriptionTest :: Spec
modifySubscriptionTest = do
    describe "modifyEventSubscription doesn't fail" $ do
        it "modifyEventSubscription doesn't throw any excpetion" $ do
            testRDS region (do
                name <- liftIO $ getRandomText "hspec-test-subscription-"
                withEventSubscription name snsTopicArn $ \_ -> do
                    modifyEventSubscription
                        (Just False)
                        ["creation","deletion"]
                        (Just snsTopicArn)
                        (Just SourceTypeDBInstance)
                        name
                ) `miss` anyConnectionException
