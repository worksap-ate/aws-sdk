{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWSTests.EC2Tests.VolumeTests
    ( runVolumeTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.EC2
import AWS.EC2.Types
import qualified AWS.EC2.Util as U
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

zone :: Text
zone = "ap-northeast-1a"

runVolumeTests :: IO ()
runVolumeTests = hspec $ do
    describeVolumesTest
    createAndDeleteVolumeTest
    describeVolumeStatusTest
    describeVolumeAttributeTest

describeVolumesTest :: Spec
describeVolumesTest = do
    describe "describeVolumes" $ do
        it "doesn't throw any exception" $ do
            testEC2 region (describeVolumes [] []) `miss` anyConnectionException

createAndDeleteVolumeTest :: Spec
createAndDeleteVolumeTest = do
    describe "{create,delete}Volume" $ do
        context "with NewVolume" $ do
            it "doesn't throw any exception" $ do
                testEC2' region (withVolume reqNew $ const (return ())) `miss` anyConnectionException
        context "with FromSnapshot" $ do
            it "doesn't throw any exception" $ do
                testEC2' region (do
                    Snapshot{snapshotId = snapshot}:_ <- U.list $ describeSnapshots [] [] [] []
                    withVolume (reqSnap snapshot) $ const (return ())
                    ) `miss` anyConnectionException

reqNew :: CreateVolumeRequest
reqNew = CreateNewVolume
    { createNewVolumeSize = 2
    , createNewVolumeAvailabilityZone = zone
    , createNewVolumeVolumeType = Nothing
    }
reqSnap :: Text -> CreateVolumeRequest
reqSnap snapshot = CreateFromSnapshot
    { createFromSnapshotSnapshotId = snapshot
    , createFromSnapshotAvailabilityZone = zone
    , createFromSnapshotSize = Nothing
    , createFromSnapshotVolumeType = Nothing
    }

describeVolumeStatusTest :: Spec
describeVolumeStatusTest = do
    describe "describeVolumeStatus" $ do
        it "doesn't throw any exception" $ do
            testEC2 region (describeVolumeStatus [] [] Nothing) `miss` anyConnectionException

describeVolumeAttributeTest :: Spec
describeVolumeAttributeTest = do
    describe "describeVolumeAttribute" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (do
                volumes <- U.list $ describeVolumes [] []
                let vid = volumeId $ head volumes
                describeVolumeAttribute vid VolumeAttributeRequestAutoEnableIO
              ) `miss` anyConnectionException
