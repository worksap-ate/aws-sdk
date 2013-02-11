{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWSTests.EC2Tests.KeyPairTests
    ( runKeyPairTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.EC2
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runKeyPairTests :: IO ()
runKeyPairTests = do
    hspec describeKeyPairsTest

describeKeyPairsTest :: Spec
describeKeyPairsTest = do
    describe "describeKeyPairs doesn't fail" $ do
        it "describeKeyPairs doesn't throw any exception" $ do
            testEC2 region (describeKeyPairs [] []) `miss` anyConnectionException
