module RDSTests.EventTests
    ( runEventTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import Cloud.AWS.RDS
import Util
import RDSTests.Util

region :: Text
region = "ap-northeast-1"

runEventTests :: IO ()
runEventTests = hspec $ do
    describeEventsTest
    describeEventCategoriesTest

describeEventsTest :: Spec
describeEventsTest = do
    describe "describeEvents doesn't fail" $ do
        it "describeEvents doesn't throw any exception" $ do
            testRDS region (
                describeEvents Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing
                ) `miss` anyConnectionException

describeEventCategoriesTest :: Spec
describeEventCategoriesTest = do
    describe "describeEventCategories doesn't fail" $ do
        it "describeEventCategories doesn't throw any exception" $ do
            testRDS region (
                describeEventCategories Nothing
                ) `miss` anyConnectionException
