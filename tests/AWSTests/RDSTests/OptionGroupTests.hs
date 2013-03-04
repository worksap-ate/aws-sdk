module AWSTests.RDSTests.OptionGroupTests
    ( runOptionGroupTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.RDS
import AWSTests.Util
import AWSTests.RDSTests.Util

region :: Text
region = "ap-northeast-1"

runOptionGroupTests :: IO ()
runOptionGroupTests = hspec $ do
    describeOptionGroupsTest
    createAndDeleteOptionGroupTest
    describeOptionGroupOptionsTest

describeOptionGroupsTest :: Spec
describeOptionGroupsTest = do
    describe "describeOptionGroups doesn't fail" $ do
        it "describeOptionGroups doesn't throw any exception" $ do
            testRDS region (
                describeOptionGroups Nothing Nothing Nothing Nothing Nothing
                ) `miss` anyConnectionException

createAndDeleteOptionGroupTest :: Spec
createAndDeleteOptionGroupTest = do
    describe "{create,delete}OptionGroup doesn't fail" $ do
        it "{create,delete}OptionGroup doesn't throw any exception" $ do
            testRDS region (do
                createOptionGroup "MySQL" "5.5" "hspec-test" name
                deleteOptionGroup name
                ) `miss` anyConnectionException
  where
    name = "hspec-test-option-group"

describeOptionGroupOptionsTest :: Spec
describeOptionGroupOptionsTest = do
    describe "describeOptionGroupOptions doesn't fail" $ do
        it "describeOptionGroupOptions doesn't throw any exception" $ do
            testRDS region (do
                describeOptionGroupOptions "oracle-se1" Nothing Nothing Nothing
                ) `miss` anyConnectionException
