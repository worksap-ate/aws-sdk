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
runSubnetsTests = hspec $ do
    describeSubnetsTest
    createAndDeleteSubnetTest

describeSubnetsTest :: Spec
describeSubnetsTest = do
    describe "describeSubnets" $ do
        it "doesn't throw any exception" $ do
            testEC2 region (describeSubnets [] []) `miss` anyConnectionException

createAndDeleteSubnetTest :: Spec
createAndDeleteSubnetTest = do
    describe "{create,delete}Subnet" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (
                withSubnet "10.0.0.0/24" $ const (return ())
                ) `miss` anyConnectionException
