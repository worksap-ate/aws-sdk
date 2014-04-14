{-# LANGUAGE FlexibleContexts #-}

module RDSTests.DBSnapshotTests
    ( runDBSnapshotTests
    )
    where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
import Data.Text (Text)
import Test.Hspec

import Cloud.AWS.RDS
import Cloud.AWS.RDS.Types
import Cloud.AWS.RDS.Util
import Util
import RDSTests.Util

region :: Text
region = "ap-northeast-1"

runDBSnapshotTests :: IO ()
runDBSnapshotTests = hspec $ do
    describeDBSnapshotsTest
    createDBSnapshotTest
    copyDBSnapshotTest

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
            testRDS region (do
                dbsid <- liftIO $ getRandomText "hspec-create-delete-"
                dbiid <- dbInstanceIdentifier . head <$>
                    describeDBInstances Nothing Nothing Nothing
                withDBSnapshot dbiid dbsid $
                    waitUntilAvailable . dbSnapshotIdentifier
                ) `miss` anyConnectionException

waitUntilAvailable :: (MonadBaseControl IO m, MonadResource m) => Text -> RDS m DBSnapshot
waitUntilAvailable = wait
    (\dbs -> dbSnapshotStatus dbs == "available")
    (\dbsid -> describeDBSnapshots Nothing (Just dbsid) Nothing Nothing Nothing)

copyDBSnapshotTest :: Spec
copyDBSnapshotTest = do
    describe "copyDBSnapshot doesn't fail" $ do
        it "copyDBSnapshot doesn't throw any exception" $ do
            testRDS region (do
                target <- liftIO $ getRandomText "hspec-copy-"
                source <- dbSnapshotIdentifier . head <$>
                    describeDBSnapshots Nothing Nothing Nothing Nothing (Just "automated")
                copyDBSnapshot source target
                waitUntilAvailable target
                deleteDBSnapshot target
                ) `miss` anyConnectionException
