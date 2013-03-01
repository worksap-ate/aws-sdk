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
runDBInstanceTests = hspec $ do
    describeDBInstancesTest
    dbInstanceTest

describeDBInstancesTest :: Spec
describeDBInstancesTest = do
    describe "describeDBInstances doesn't fail" $ do
        it "describeDBInstances doesn't throw any exception" $ do
            testRDS region (
                describeDBInstances Nothing Nothing Nothing
                ) `miss` anyConnectionException

dbInstanceTest :: Spec
dbInstanceTest = do
    describe "DBInstance operations don't fail" $ do
        it "DBInstance operations don't throw any exception" $ do
            dbiid <- getRandomText "hspec-dbinstance-"
            replicaId <- getRandomText "hspec-replica-"
            finalSnapshot <- getRandomText "hspec-final-snapshot-"
            testRDS region (do
                withDBInstance (createTestDBInstanceRequest dbiid) $ \_ -> do
                    -- deleteDBInstance with FinalSnapshot
                    waitUntilAvailable dbiid
                    deleteDBInstance dbiid (FinalSnapshotIdentifier finalSnapshot)

                    -- restoreDBInstanceFromDBSnapshot
                    wait
                        (\dbs -> dbSnapshotStatus dbs == "available")
                        (\dbsid -> describeDBSnapshots Nothing (Just dbsid) Nothing Nothing Nothing)
                        finalSnapshot
                    waitUntilNotFound
                        (describeDBInstances Nothing Nothing Nothing)
                        (\dbi -> dbInstanceIdentifier dbi == dbiid)
                        (\dbi -> dbInstanceStatus dbi == Just "available")
                        (\_ -> deleteDBInstance dbiid SkipFinalSnapshot)
                    restoreDBInstanceFromDBSnapshot $
                        restoreDBInstanceRequest finalSnapshot dbiid

                    -- rebootDBInstance
                    waitUntilAvailable dbiid
                    rebootDBInstance dbiid Nothing

                    -- createDBInstanceReadReplica
                    waitUntilAvailable dbiid
                    let replicaReq = CreateReadReplicaRequest
                            Nothing Nothing
                            "db.t1.micro"
                            replicaId
                            Nothing Nothing Nothing Nothing
                            dbiid
                    createDBInstanceReadReplica replicaReq

                    -- promoteReadReplica
                    waitUntilAvailable dbiid
                    waitUntilAvailable replicaId
                    promoteReadReplica Nothing replicaId Nothing
                    waitUntilAvailable replicaId
                    deleteDBInstance replicaId SkipFinalSnapshot

                    -- deleteDBSnapshot
                    deleteDBSnapshot finalSnapshot
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

restoreDBInstanceRequest :: Text -> Text -> RestoreDBInstanceFromDBSnapshotRequest
restoreDBInstanceRequest dbsid dbiid = RestoreDBInstanceFromDBSnapshotRequest
    Nothing Nothing Nothing dbiid Nothing dbsid Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
