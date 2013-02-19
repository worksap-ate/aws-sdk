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
    hspec createDBInstanceReadReplicaTest
    hspec rebootDBInstanceTest

describeDBInstancesTest :: Spec
describeDBInstancesTest = do
    describe "describeDBInstances doesn't fail" $ do
        it "describeDBInstances doesn't throw any exception" $ do
            testRDS region (
                describeDBInstances Nothing Nothing Nothing
                ) `miss` anyConnectionException

createAndDeleteDBInstanceTest :: Spec
createAndDeleteDBInstanceTest = do
    describe "{create,delete}DBInstance doesn't fail" $ do
        it "{create,delete}DBInstance doesn't throw any exception" $ do
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

createDBInstanceReadReplicaTest :: Spec
createDBInstanceReadReplicaTest = do
    describe "createDBInstanceReadReplica doesn't fail" $ do
        it "createDBInstanceReadReplica doesn't throw any exception" $ do
            testRDS region (do
                withDBInstance createTestDBInstanceRequest $ \dbi -> do
                    waitUntilAvailable $ dbInstanceIdentifier dbi
                    let replicaReq = CreateReadReplicaRequest
                            Nothing Nothing
                            "db.t1.micro"
                            "hspec-test-replica"
                            Nothing Nothing Nothing Nothing
                            "hspec-test-instance"
                    replica <- createDBInstanceReadReplica replicaReq
                    waitUntilAvailable $ dbInstanceIdentifier dbi
                    waitUntilAvailable $ dbInstanceIdentifier replica
                    deleteDBInstance (dbInstanceIdentifier replica) SkipFinalSnapshot
                ) `miss` anyConnectionException

rebootDBInstanceTest :: Spec
rebootDBInstanceTest = do
    describe "rebootDBInstance doesn't fail" $ do
        it "rebootDBInstance doesn't throw any exception" $ do
            testRDS region (do
                withDBInstance createTestDBInstanceRequest $ \dbi -> do
                    let dbiid = dbInstanceIdentifier dbi
                    waitUntilAvailable dbiid
                    rebootDBInstance dbiid Nothing
                    waitUntilAvailable dbiid
                ) `miss` anyConnectionException
