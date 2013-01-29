module AWSTests.RDSTests.DBSecurityGroupTests
    ( runDBSecurityGroupTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.RDS
import AWSTests.Util
import AWSTests.RDSTests.Util

region :: Text
region = "ap-northeast-1"

runDBSecurityGroupTests :: IO ()
runDBSecurityGroupTests = do
    hspec describeDBSecurityGroupsTest

describeDBSecurityGroupsTest :: Spec
describeDBSecurityGroupsTest = do
    describe "describeDBSecurityGroups doesn't fail" $ do
        it "describeDBSecurityGroups doesn't throw any exception" $ do
            testRDS region (describeDBSecurityGroups Nothing Nothing Nothing)
                `miss` anyHttpException
