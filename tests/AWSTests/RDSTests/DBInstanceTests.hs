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
            dbiid <- getRandomText "hspec-create-delete-"
            testRDS region (
                withDBInstance (createTestDBInstanceRequest dbiid) $
                    waitUntilAvailable . dbInstanceIdentifier
                ) `miss` anyConnectionException

waitUntilAvailable :: (MonadBaseControl IO m, MonadResource m) => Text -> RDS m DBInstance
waitUntilAvailable = wait
    (\dbi -> dbInstanceStatus dbi == Just "available")
    (\dbiid -> describeDBInstances (Just dbiid) Nothing Nothing)

createTestDBInstanceRequest :: Text -> CreateDBInstanceRequest
createTestDBInstanceRequest dbiid = CreateDBInstanceRequest
    5
    Nothing Nothing Nothing Nothing
    "db.t1.micro"
    dbiid
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
            originId <- getRandomText "hspec-replica-origin-"
            replicaId <- getRandomText "hspec-replica-replica-"
            testRDS region (do
                withDBInstance (createTestDBInstanceRequest originId) $ \_ -> do
                    waitUntilAvailable originId
                    let replicaReq = CreateReadReplicaRequest
                            Nothing Nothing
                            "db.t1.micro"
                            replicaId
                            Nothing Nothing Nothing Nothing
                            originId
                    createDBInstanceReadReplica replicaReq
                    waitUntilAvailable originId
                    waitUntilAvailable replicaId
                    deleteDBInstance replicaId SkipFinalSnapshot
                ) `miss` anyConnectionException

rebootDBInstanceTest :: Spec
rebootDBInstanceTest = do
    describe "rebootDBInstance doesn't fail" $ do
        it "rebootDBInstance doesn't throw any exception" $ do
            dbiid <- getRandomText "hspec-reboot-"
            testRDS region (do
                withDBInstance (createTestDBInstanceRequest dbiid) $ \_ -> do
                    waitUntilAvailable dbiid
                    rebootDBInstance dbiid Nothing
                    waitUntilAvailable dbiid
                ) `miss` anyConnectionException
