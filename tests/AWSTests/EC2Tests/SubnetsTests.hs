{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWSTests.EC2Tests.SubnetsTests
    ( runSubnetsTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.EC2
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runSubnetsTests :: IO ()
runSubnetsTests = do
    hspec describeSubnetsTest

describeSubnetsTest :: Spec
describeSubnetsTest = do
    describe "describeSubnets doesn't fail" $ do
        it "describeSubnets doesn't throw any exception" $ do
            testEC2 region (describeSubnets [] []) `miss` anyConnectionException
