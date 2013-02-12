module AWSTests.RDSTests.DBParameterGroupTests
    ( runDBParameterGroupTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.RDS
import AWSTests.Util
import AWSTests.RDSTests.Util

region :: Text
region = "ap-northeast-1"

runDBParameterGroupTests :: IO ()
runDBParameterGroupTests = do
    hspec describeDBParameterGroupsTest
    hspec createAndDeleteDBParameterGroupTest

describeDBParameterGroupsTest :: Spec
describeDBParameterGroupsTest = do
    describe "describeDBParameterGroups doesn't fail" $ do
        it "describeDBParameterGroups doesn't throw any exception" $ do
            testRDS region (describeDBParameterGroups Nothing Nothing Nothing)
                `miss` anyHttpException

createAndDeleteDBParameterGroupTest :: Spec
createAndDeleteDBParameterGroupTest = do
    describe "{create,delete}DBParameterGroup doesn't fail" $ do
        it "{create,delete}DBParameterGroup doesn't throw any exception" $ do
            testRDS region (do
                createDBParameterGroup "MySQL5.5" name "test"
                deleteDBParameterGroup name
                ) `miss` anyHttpException
  where
    name = "hspec-test-parameter-group"
