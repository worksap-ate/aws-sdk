{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWSTests.EC2Tests.VolumeTests
    ( runVolumeTests
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

runVolumeTests :: IO ()
runVolumeTests = do
    hspec describeVolumesTest
    hspec describeVolumeStatusTest
    hspec describeVolumeAttributeTest

describeVolumesTest :: Spec
describeVolumesTest = do
    describe "describeVolumes doesn't fail" $ do
        it "describeVolumes doesn't throw any exception" $ do
            testEC2 region (describeVolumes [] []) `miss` anyHttpException

describeVolumeStatusTest :: Spec
describeVolumeStatusTest = do
    describe "describeVolumeStatus doesn't fail" $ do
        it "describeVolumeStatus doesn't throw any exception" $ do
            testEC2 region (describeVolumeStatus [] [] Nothing) `miss` anyHttpException

describeVolumeAttributeTest :: Spec
describeVolumeAttributeTest = do
    describe "describeVolumeAttribute doesn't fail" $ do
        it "describeVolumeAttribute doesn't throw any exception" $ do
            volumes <- testEC2 region (describeVolumes [] [])
            let vid = volumeId $ head volumes
            testEC2' region (describeVolumeAttribute vid VolumeAttributeRequestAutoEnableIO) `miss` anyHttpException
