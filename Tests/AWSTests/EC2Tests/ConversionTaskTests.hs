{-# LANGUAGE FlexibleContexts #-}

module AWSTests.EC2Tests.ConversionTaskTests
    ( runConversionTaskTests
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

runConversionTaskTests :: IO ()
runConversionTaskTests = do
    hspec describeConversionTasksTest
    hspec importVolumeTest
    hspec importInstanceTest

describeConversionTasksTest :: Spec
describeConversionTasksTest = do
    describe "describeConversionTasks doesn't fail" $ do
        it "describeConversionTasks doesn't throw any exception" $ do
            testEC2 region (describeConversionTasks []) `miss` anyHttpException

importVolumeTest :: Spec
importVolumeTest = do
    describe "importVolume doesn't fail" $ do
        it "importVolume doesn't throw any exception" $ do
            task <- testEC2' region test
            let taskId = conversionTaskId task
            testEC2' region (cancelConversionTask taskId)
                `shouldReturn` True
  where
    test = importVolume
        "ap-northeast-1a"
        (ImportVolumeRequestImage "RAW" 8 "https://test.test")
        Nothing
        8

importInstanceTest :: Spec
importInstanceTest = do
    describe "importInstance doesn't fail" $ do
        it "importInstance doesn't throw any exception" $ do
            task <- testEC2' region test
            let taskId = conversionTaskId task
            testEC2' region (cancelConversionTask taskId)
                `shouldReturn` True
  where
    test = importInstance
        Nothing
        (LaunchSpecification I386 [] Nothing "m1.small" Nothing Nothing Nothing Nothing Nothing)
        [DiskImage "VMDK" 1024 "https://test.test" Nothing 2]
        PlatformWindows
