module AWSTests.RDSTests.DBSubnetGroupTests
    ( runDBSubnetGroupTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.RDS
import AWSTests.Util
import AWSTests.RDSTests.Util

region :: Text
region = "ap-northeast-1"

runDBSubnetGroupTests :: IO ()
runDBSubnetGroupTests = do
    hspec describeDBSubnetGroupsTest

describeDBSubnetGroupsTest :: Spec
describeDBSubnetGroupsTest = do
    describe "describeDBSubnetGroups doesn't fail" $ do
        it "describeDBSubnetGroups doesn't throw any exception" $ do
            testRDS region (describeDBSubnetGroups Nothing Nothing Nothing) `miss` anyHttpException
