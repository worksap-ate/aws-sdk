module AWSTests.RDSTests.DBSnapshotTests
    ( runDBSnapshotTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.RDS
import AWS.RDS.Types
import AWS.RDS.Util
import AWSTests.Util
import AWSTests.RDSTests.Util

region :: Text
region = "ap-northeast-1"

runDBSnapshotTests :: IO ()
runDBSnapshotTests = do
    hspec describeDBSnapshotsTest
    hspec createDBSnapshotTest

describeDBSnapshotsTest :: Spec
describeDBSnapshotsTest = do
    describe "describeDBSnapshots doesn't fail" $ do
        it "describeDBSnapshots doesn't throw any exception" $ do
            testRDS region (
                describeDBSnapshots Nothing Nothing Nothing Nothing Nothing
                ) `miss` anyHttpException

createDBSnapshotTest :: Spec
createDBSnapshotTest = do
    describe "{create,delete}DBSnapshot doesn't fail" $ do
        it "{create,delete}DBSnapshot doesn't throw any exception" $ do
            testRDS region test `miss` anyHttpException
  where
    dbsid = "hspec-test-snapshot"
    test = do
        dbis <- describeDBInstances Nothing Nothing Nothing
        let dbiid = dbiDBInstanceIdentifier $ head dbis
        dbs <- createDBSnapshot dbiid dbsid
        wait
            (\dbs' -> dbsStatus dbs' == "available")
            (\dbsid' -> describeDBSnapshots Nothing (Just dbsid') Nothing Nothing Nothing) $
            dbsDBSnapshotIdentifier dbs
        deleteDBSnapshot dbsid
