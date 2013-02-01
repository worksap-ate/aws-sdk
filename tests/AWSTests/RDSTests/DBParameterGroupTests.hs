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

describeDBParameterGroupsTest :: Spec
describeDBParameterGroupsTest = do
    describe "describeDBParameterGroups doesn't fail" $ do
        it "describeDBParameterGroups doesn't throw any exception" $ do
            testRDS region (describeDBParameterGroups Nothing Nothing Nothing)
                `miss` anyHttpException
