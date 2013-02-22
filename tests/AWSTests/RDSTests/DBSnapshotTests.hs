{-# LANGUAGE FlexibleContexts #-}

module AWSTests.RDSTests.DBSnapshotTests
    ( runDBSnapshotTests
    )
    where

import Control.Applicative ((<$>))
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
                ) `miss` anyConnectionException

createDBSnapshotTest :: Spec
createDBSnapshotTest = do
    describe "{create,delete}DBSnapshot doesn't fail" $ do
        it "{create,delete}DBSnapshot doesn't throw any exception" $ do
            dbsid <- getRandomText "hspec-create-delete-"
            testRDS region (do
                dbiid <- dbInstanceIdentifier . head <$>
                    describeDBInstances Nothing Nothing Nothing
                withDBSnapshot dbiid dbsid $
                    waitUntilAvailable . dbSnapshotIdentifier
                ) `miss` anyConnectionException

waitUntilAvailable :: (MonadBaseControl IO m, MonadResource m) => Text -> RDS m DBSnapshot
waitUntilAvailable = wait
    (\dbs -> dbSnapshotStatus dbs == "available")
    (\dbsid -> describeDBSnapshots Nothing (Just dbsid) Nothing Nothing Nothing)
