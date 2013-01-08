{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWSTests.EC2Tests.SnapshotTests
    ( runSnapshotTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.EC2
import AWS.EC2.Types
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runSnapshotTests :: IO ()
runSnapshotTests = do
    hspec describeSnapshotsTest
    hspec createSnapshotTest
    hspec describeSnapshotAttributeTest

describeSnapshotsTest :: Spec
describeSnapshotsTest = do
    describe "describeSnapshots doesn't fail" $ do
        it "describeSnapshots doesn't throw any exception" $ do
            testEC2 region (describeSnapshots [] [] [] []) `miss` anyHttpException

createSnapshotTest :: Spec
createSnapshotTest = do
    describe "createSnapshot doesn't fail" $ do
        it "createSnapshot, deleteSnapshot and copySnapshot doesn't any exception" $ do
            volumes <- testEC2 region (describeVolumes [] [])
            let vid = volumeId $ head volumes
            snapshot <- testEC2' region (createSnapshot vid Nothing)
            let sid = snapshotId snapshot
            copyId <- testEC2' region (copySnapshot region sid Nothing)
            testEC2' region (deleteSnapshot sid) `shouldReturn` True
            testEC2' region (deleteSnapshot copyId) `shouldReturn` True

describeSnapshotAttributeTest :: Spec
describeSnapshotAttributeTest = do
    describe "describeSnapshotAttribute doesn't fail" $ do
        it "describeSnapshotAttribute doesn't throw any exception" $ do
            snapshots <- testEC2 region (describeSnapshots [] [] [] [])
            let sid = snapshotId $ head snapshots
            testEC2' region (describeSnapshotAttribute sid SnapshotAttributeRequestCreateVolumePermission) `miss` anyHttpException
