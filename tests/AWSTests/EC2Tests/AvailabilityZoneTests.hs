{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWSTests.EC2Tests.AvailabilityZoneTests
    ( runAvailabilityZoneTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import Cloud.AWS.EC2
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runAvailabilityZoneTests :: IO ()
runAvailabilityZoneTests = do
    hspec describeAvailabilityZonesTest

describeAvailabilityZonesTest :: Spec
describeAvailabilityZonesTest = do
    describe "describeAvailabilityZones doesn't fail" $ do
        it "describeAvailabilityZones doesn't throw any exception" $ do
            testEC2 region (describeAvailabilityZones [] []) `miss` anyConnectionException
