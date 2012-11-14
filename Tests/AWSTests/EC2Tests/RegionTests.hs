{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWSTests.EC2Tests.RegionTests
    ( runRegionTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.EC2
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runRegionTests :: IO ()
runRegionTests = do
    hspec describeRegionsTest

describeRegionsTest :: Spec
describeRegionsTest = do
    describe "describeRegions doesn't fail" $ do
        it "describeRegions doesn't throw any exception" $ do
            testEC2 region (describeRegions [] []) `miss` anyHttpException
