{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module EC2Tests.SnapshotTests
    ( runSnapshotTests
    )
    where

import qualified Control.Exception.Lifted as E
import Data.Text (Text)
import Test.Hspec

import Cloud.AWS.EC2
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Util (list)
import Util
import EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runSnapshotTests :: IO ()
runSnapshotTests = hspec $ do
    describeSnapshotsTest
    createSnapshotTest
    describeSnapshotAttributeTest

describeSnapshotsTest :: Spec
describeSnapshotsTest = do
    describe "describeSnapshots doesn't fail" $ do
        it "describeSnapshots doesn't throw any exception" $ do
            testEC2 region (describeSnapshots [] [] [] []) `miss` anyConnectionException

createSnapshotTest :: Spec
createSnapshotTest = do
    describe "createSnapshot doesn't fail" $ do
        it "createSnapshot, deleteSnapshot and copySnapshot doesn't any exception" $ do
            testEC2' region (do
                Volume{volumeId = vid}:_ <- list $ describeVolumes [] []
                withSnapshot vid Nothing $ \Snapshot{snapshotId = sid} ->
                    E.bracket (copySnapshot region sid Nothing) deleteSnapshot $ \copied -> do
                        modifySnapshotAttribute copied perm
                        describeSnapshotAttribute copied SnapshotAttributeRequestCreateVolumePermission
                        resetSnapshotAttribute copied ResetSnapshotAttributeRequestCreateVolumePermission
                ) `miss` anyConnectionException
  where
    perm = CreateVolumePermission
        { createVolumePermissionAdd = [item1, item2, item3]
        , createVolumePermissionRemove = []
        }
    item1 = CreateVolumePermissionItemUserId "111122223333"
    item2 = CreateVolumePermissionItemGroup "all"
    item3 = CreateVolumePermissionItemUserId "333322221111"

describeSnapshotAttributeTest :: Spec
describeSnapshotAttributeTest = do
    describe "describeSnapshotAttribute doesn't fail" $ do
        it "describeSnapshotAttribute doesn't throw any exception" $ do
            testEC2' region (do
                snapshots <- list $ describeSnapshots [] [] [] []
                let sid = snapshotId $ head snapshots
                describeSnapshotAttribute sid SnapshotAttributeRequestCreateVolumePermission
                ) `miss` anyConnectionException
