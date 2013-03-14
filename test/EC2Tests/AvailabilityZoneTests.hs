{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module EC2Tests.AvailabilityZoneTests
    ( runAvailabilityZoneTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import Cloud.AWS.EC2
import Util
import EC2Tests.Util

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
