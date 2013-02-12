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
    hspec createAndDeleteDBSecurityGroupTest

describeDBSecurityGroupsTest :: Spec
describeDBSecurityGroupsTest = do
    describe "describeDBSecurityGroups doesn't fail" $ do
        it "describeDBSecurityGroups doesn't throw any exception" $ do
            testRDS region (describeDBSecurityGroups Nothing Nothing Nothing)
                `miss` anyHttpException

createAndDeleteDBSecurityGroupTest :: Spec
createAndDeleteDBSecurityGroupTest = do
    describe "{create,delete}DBSecurityGroup doesn't fail" $ do
        it "{create,delete}DBSecurityGroup doesn't throw any exception" $ do
            testRDS region (do
                createDBSecurityGroup name "test"
                deleteDBSecurityGroup name
                ) `miss` anyHttpException
  where
    name = "hspec-test-security-group"
