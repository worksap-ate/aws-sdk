module AWSTests.RDSTests.DBSubnetGroupTests
    ( runDBSubnetGroupTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.RDS
import AWS.RDS.Types (DBSubnetGroup(..), Subnet(..))
import AWSTests.Util
import AWSTests.RDSTests.Util

region :: Text
region = "ap-northeast-1"

runDBSubnetGroupTests :: IO ()
runDBSubnetGroupTests = do
    hspec describeDBSubnetGroupsTest
    hspec createAndDeleteDBSubnetGroupTest

describeDBSubnetGroupsTest :: Spec
describeDBSubnetGroupsTest = do
    describe "describeDBSubnetGroups doesn't fail" $ do
        it "describeDBSubnetGroups doesn't throw any exception" $ do
            testRDS region (describeDBSubnetGroups Nothing Nothing Nothing)
                `miss` anyConnectionException

createAndDeleteDBSubnetGroupTest :: Spec
createAndDeleteDBSubnetGroupTest = do
    describe "{create,delete}DBSubnetGroup doesn't fail" $ do
        it "{create,delete}DBSubnetGroup doesn't throw any exception" $ do
            testRDS region (do
                sgs <- describeDBSubnetGroups Nothing Nothing Nothing
                createDBSubnetGroup name (subnets sgs) "test"
                deleteDBSubnetGroup name
                ) `miss` anyConnectionException
  where
    name = "hspec-test-subnet-group"
    subnets = map subnetIdentifier . dbSubnets . head
