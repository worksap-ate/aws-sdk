{-# LANGUAGE FlexibleContexts #-}

module AWSTests.EC2Tests.ConversionTaskTests
    ( runConversionTaskTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.EC2
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runConversionTaskTests :: IO ()
runConversionTaskTests = do
    hspec describeConversionTasksTest

describeConversionTasksTest :: Spec
describeConversionTasksTest = do
    describe "describeConversionTasks doesn't fail" $ do
        it "describeConversionTasks doesn't throw any exception" $ do
            testEC2 region (describeConversionTasks []) `miss` anyHttpException
