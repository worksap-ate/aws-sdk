{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWSTests.EC2Tests.VPCTests
    ( runVpcTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.EC2
import AWS.EC2.Types
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-southeast-1"

runVpcTests :: IO ()
runVpcTests = do
    hspec describeVpcsTest
    hspec describeVpnGatewaysTest
    hspec describeInternetGatewaysTest
    hspec describeVpnConnectionsTest
    hspec describeCustomerGatewayTest
    hspec describeInternetGatewaysTest
    -- hspec createVpcTest

describeVpcsTest :: Spec
describeVpcsTest = do
    describe "describeVpcs doesn't fail" $ do
        it "describeVpcs doesn't throw any exception" $ do
            testEC2 region (describeVpcs [] []) `shouldntThrow` anyException

describeVpnGatewaysTest :: Spec
describeVpnGatewaysTest = do
    describe "describeVpnGateways doesn't fail" $ do
        it "describeVpnGateways doesn't throw any exception" $ do
            testEC2 region (describeVpnGateways [] []) `shouldntThrow` anyException

describeVpnConnectionsTest :: Spec
describeVpnConnectionsTest = do
    describe "describeVpnConnections doesn't fail" $ do
        it "describeVpnConnections doesn't throw any exception" $ do
            testEC2 region (describeVpnConnections [] []) `shouldntThrow` anyException

describeCustomerGatewayTest :: Spec
describeCustomerGatewayTest = do
    describe "describeCustomerGateway doesn't fail" $ do
        it "describeCustomerGateway doesn't throw any exception" $ do
            testEC2 region (describeCustomerGateway [] []) `shouldntThrow` anyException

describeInternetGatewaysTest :: Spec
describeInternetGatewaysTest = do
    describe "describeInternetGateways doesn't fail" $ do
        it "describeInternetGateways doesn't throw any exception" $ do
            testEC2 region (describeInternetGateways [] []) `shouldntThrow` anyException

createVpcTest :: Spec
createVpcTest = do
    describe "createVpc doesn't fail" $ do
        it "createVpc and deleteVpc doesn't fail" $ do
            vpc <- testEC2' region (createVpc "80.0.0.0/16" Nothing)
            testEC2' region (deleteVpc $ vpcId vpc) `shouldReturn` True
