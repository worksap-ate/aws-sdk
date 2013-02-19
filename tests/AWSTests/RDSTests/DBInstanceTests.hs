{-# LANGUAGE FlexibleContexts #-}

module AWSTests.RDSTests.DBInstanceTests
    ( runDBInstanceTests
    )
    where

import Data.Conduit
import Data.Text (Text)
import Test.Hspec

import AWS.RDS
import AWS.RDS.Types
import AWS.RDS.Util
import AWSTests.Util
import AWSTests.RDSTests.Util

region :: Text
region = "ap-northeast-1"

runDBInstanceTests :: IO ()
runDBInstanceTests = do
    hspec describeDBInstancesTest
    hspec createAndDeleteDBInstanceTest

describeDBInstancesTest :: Spec
describeDBInstancesTest = do
    describe "describeDBInstances doesn't fail" $ do
        it "describeDBInstances doesn't throw any exception" $ do
            testRDS region (describeDBInstances Nothing Nothing Nothing) `miss` anyConnectionException

createAndDeleteDBInstanceTest :: Spec
createAndDeleteDBInstanceTest = do
    describe "{create,delete}DBInstance doesn't fail" $ do
        it "{create,delete}DBInstance doesn't any exception" $ do
            testRDS region (
                withDBInstance createTestDBInstanceRequest $
                    waitUntilAvailable . dbInstanceIdentifier
                ) `miss` anyConnectionException

waitUntilAvailable :: (MonadBaseControl IO m, MonadResource m) => Text -> RDS m DBInstance
waitUntilAvailable = wait
    (\dbi -> dbInstanceStatus dbi == Just "available")
    (\dbiid -> describeDBInstances (Just dbiid) Nothing Nothing)

createTestDBInstanceRequest :: CreateDBInstanceRequest
createTestDBInstanceRequest = CreateDBInstanceRequest
    5
    Nothing Nothing Nothing Nothing
    "db.t1.micro"
    "hspec-test-instance"
    Nothing Nothing [] Nothing
    "MySQL"
    Nothing Nothing Nothing
    "test"
    "test"
    Nothing Nothing Nothing Nothing Nothing Nothing []
