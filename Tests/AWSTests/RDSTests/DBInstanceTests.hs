module AWSTests.RDSTests.DBInstanceTests
    ( runDBInstanceTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.RDS
import AWSTests.Util
import AWSTests.RDSTests.Util

region :: Text
region = "ap-northeast-1"

runDBInstanceTests :: IO ()
runDBInstanceTests = do
    hspec describeDBInstancesTest

describeDBInstancesTest :: Spec
describeDBInstancesTest = do
    describe "describeDBInstances doesn't fail" $ do
        it "describeDBInstances doesn't throw any exception" $ do
            testRDS region (describeDBInstances Nothing Nothing Nothing) `miss` anyHttpException
