module RDSTests.DBSubnetGroupTests
    ( runDBSubnetGroupTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import Cloud.AWS.RDS
import Cloud.AWS.RDS.Types (DBSubnetGroup(..), Subnet(..))
import Util
import RDSTests.Util

region :: Text
region = "ap-northeast-1"

runDBSubnetGroupTests :: IO ()
runDBSubnetGroupTests = do
    hspec describeDBSubnetGroupsTest
    hspec createDeleteModifyDBSubnetGroupTest

describeDBSubnetGroupsTest :: Spec
describeDBSubnetGroupsTest = do
    describe "describeDBSubnetGroups doesn't fail" $ do
        it "describeDBSubnetGroups doesn't throw any exception" $ do
            testRDS region (describeDBSubnetGroups Nothing Nothing Nothing)
                `miss` anyConnectionException

createDeleteModifyDBSubnetGroupTest :: Spec
createDeleteModifyDBSubnetGroupTest = do
    describe "{create,delete,modify}DBSubnetGroup doesn't fail" $ do
        it "{create,delete,modify}DBSubnetGroup doesn't throw any exception" $ do
            testRDS region (do
                sgs <- describeDBSubnetGroups Nothing Nothing Nothing
                createDBSubnetGroup name (subnets sgs) "test"
                modifyDBSubnetGroup name (Just "test-modified") (subnets sgs)
                deleteDBSubnetGroup name
                ) `miss` anyConnectionException
  where
    name = "hspec-test-subnet-group"
    subnets = map subnetIdentifier . dbSubnets . head
