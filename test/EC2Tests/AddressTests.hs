{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module EC2Tests.AddressTests
    ( runAddressTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import Cloud.AWS.EC2
import Cloud.AWS.EC2.Types
import Util
import EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runAddressTests :: IO ()
runAddressTests = hspec $ do
    describe "describeAddresses" $ do
        it "doesn't throw any exception" $ do
            testEC2 region (describeAddresses [] [] [])
                `miss` anyConnectionException

    describe "{allocate,release}Address" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (do
                addr <- allocateAddress False
                releaseAddress (Just $ allocateAddressPublicIp addr) Nothing
                addr2 <- allocateAddress True
                releaseAddress Nothing (allocateAddressAllocationId addr2)
                ) `miss` anyConnectionException

    describe "[dis]associateAddress" $ do
        context "with EC2" $ do
            it "doesn't throw any exception" $ do
                testEC2' region (
                    withInstance testRunInstancesRequest $ \Instance{instanceId = inst} ->
                        withAddress False $ \AllocateAddress{allocateAddressPublicIp = ip} -> do
                            waitForInstanceState InstanceStateRunning inst
                            associateAddress $ AssociateAddressRequestEc2 ip inst
                            disassociateAddress $ DisassociateAddressRequestEc2 ip
                    ) `miss` anyConnectionException

        context "with VPC" $ do
            it "doesn't throw any exception" $ do
                testEC2' region (
                    withSubnet "10.0.0.0/24" $ \Subnet{subnetId = subnet, subnetVpcId = vpc} ->
                        setupNetworkInterface vpc subnet $ \nic ->
                            withAddress True $ \AllocateAddress{allocateAddressAllocationId = Just alloc} -> do
                                (_, Just assoc) <- associateAddress $ AssociateAddressRequestVpc
                                    { associateAddressRequestVpcAllocationId = alloc
                                    , associateAddressRequestVpcInstanceId = Nothing
                                    , associateAddressRequestVpcNetworkInterfaceId = Just nic
                                    , associateAddressRequestVpcPrivateIpAddress = Nothing
                                    , associateAddressRequestVpcAllowReassociation = Nothing
                                    }
                                disassociateAddress $ DisassociateAddressRequestVpc assoc
                    ) `miss` anyConnectionException
  where
    setupNetworkInterface vpc subnet f =
        withNetworkInterface subnet $ \NetworkInterface{networkInterfaceId = nic} ->
            withInternetGateway $ \InternetGateway{internetGatewayInternetGatewayId = igw} ->
                withInternetGatewayAttached igw vpc $
                    f nic
