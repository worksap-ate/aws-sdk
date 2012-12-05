{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWSTests.EC2Tests.NetworkInterfaceTests
    ( runNetworkInterfaceTests
    ) where

import Data.Text (Text)
import Test.Hspec

import AWS.EC2
--import AWS.EC2.Types
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runNetworkInterfaceTests :: IO ()
runNetworkInterfaceTests = do
    hspec describeNetworkInterfacesTest

describeNetworkInterfacesTest :: Spec
describeNetworkInterfacesTest = do
    describe "describeNetworkInterfaces doesn't fail" $ do
        it "describeNetworkInterfaces doesn't throw any exception" $ do
            testEC2 region (describeNetworkInterfaces [] []) `miss` anyHttpException
