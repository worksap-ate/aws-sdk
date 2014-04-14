{-# LANGUAGE FlexibleContexts #-}

module RDSTests.DBInstanceTests
    ( runDBInstanceTests
    )
    where

import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
import Data.Monoid ((<>))
import Data.Text (Text)
import Test.Hspec

import Cloud.AWS.RDS
import Cloud.AWS.RDS.Types
import Cloud.AWS.RDS.Util
import Util
import RDSTests.Util

region :: Text
region = "ap-northeast-1"

runDBInstanceTests :: IO ()
runDBInstanceTests = hspec $ do
    describeDBInstancesTest
    dbInstanceTest
    describeOrderableDBInstanceOptionsTest
    describeReservedDBInstancesOfferingsTest

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
            restoreId <- getRandomText "hspec-restore-point-in-time-"
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

                    -- restoreDBInstanceToPointInTime
                    restoreDBInstanceToPointInTime
                        UseLatestRestorableTime $
                        restoreDBInstanceToPointInTimeRequest dbiid restoreId
                    waitUntilAvailable dbiid
                    waitUntilAvailable restoreId
                    deleteDBInstance restoreId SkipFinalSnapshot

                    -- modifyDBInstance
                    modifyDBInstance $ modifyTestDBInstanceRequest dbiid

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
    "testtesttesttest"
    "test"
    Nothing Nothing Nothing Nothing Nothing Nothing []

restoreDBInstanceRequest :: Text -> Text -> RestoreDBInstanceFromDBSnapshotRequest
restoreDBInstanceRequest dbsid dbiid = RestoreDBInstanceFromDBSnapshotRequest
    Nothing Nothing Nothing dbiid Nothing dbsid Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing

restoreDBInstanceToPointInTimeRequest :: Text -> Text -> RestoreDBInstanceToPointInTimeRequest
restoreDBInstanceToPointInTimeRequest source target =
    RestoreDBInstanceToPointInTimeRequest
        source target
        Nothing Nothing Nothing Nothing Nothing Nothing
        Nothing Nothing Nothing Nothing Nothing Nothing

modifyTestDBInstanceRequest :: Text -> ModifyDBInstanceRequest
modifyTestDBInstanceRequest dbiid = ModifyDBInstanceRequest
    Nothing Nothing Nothing Nothing Nothing
    Nothing dbiid Nothing [] Nothing Nothing
    Nothing Nothing (Just $ dbiid <> "modified")
    Nothing Nothing Nothing []

describeOrderableDBInstanceOptionsTest :: Spec
describeOrderableDBInstanceOptionsTest = do
    describe "describeOrderableDBInstanceOptions doesn't fail" $ do
        it "describeOrderableDBInstanceOptions doesn't throw any exception" $ do
            testRDS region (do
                describeOrderableDBInstanceOptions Nothing "mysql"
                    Nothing Nothing Nothing Nothing Nothing
                ) `miss` anyConnectionException

describeReservedDBInstancesOfferingsTest :: Spec
describeReservedDBInstancesOfferingsTest = do
    describe "describeReservedDBInstancesOfferingTest doesn't fail" $ do
        it "describeReservedDBInstancesOfferingsTest doesn't throw any exception" $ do
            testRDS region (do
                describeReservedDBInstancesOfferings
                    Nothing Nothing Nothing Nothing
                    Nothing Nothing Nothing Nothing
                ) `miss` anyConnectionException
