{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module EC2Tests.NetworkInterfaceTests
    ( runNetworkInterfaceTests
    ) where

import Data.Text (Text)
import Test.Hspec

import Cloud.AWS.EC2
import Cloud.AWS.EC2.Types
import Util
import EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runNetworkInterfaceTests :: IO ()
runNetworkInterfaceTests = hspec $ do
    describeNetworkInterfacesTest
    createAndDeleteNetworkInterfaceTest
    attachAndDetachNetworkInterfaceTest
    assignPrivateIpAddressesTest
    unassignPrivateIpAddressesTest
    runInstanceTest

describeNetworkInterfacesTest :: Spec
describeNetworkInterfacesTest = do
    describe "describeNetworkInterfaces doesn't fail" $ do
        it "describeNetworkInterfaces doesn't throw any exception" $ do
            testEC2 region (describeNetworkInterfaces [] []) `miss` anyConnectionException

createAndDeleteNetworkInterfaceTest :: Spec
createAndDeleteNetworkInterfaceTest = do
    describe "{create,delete}NetworkInterface" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (
                withSubnet "10.0.0.0/24" $ \Subnet{subnetId = subnet} ->
                    withNetworkInterface subnet $ \_ -> return ()
                ) `miss` anyConnectionException

attachAndDetachNetworkInterfaceTest :: Spec
attachAndDetachNetworkInterfaceTest = do
    describe "{attach,detach}NetworkInterface" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (
                withSubnet "10.0.0.0/24" $ \Subnet{subnetId = subnet} -> do
                    i <- withInstance (req subnet) $ \Instance{instanceId = inst} -> do
                        withNetworkInterface subnet $ \NetworkInterface{networkInterfaceId = nic} -> do
                            waitForInstanceState InstanceStateRunning inst
                            attachment <- attachNetworkInterface nic inst 1
                            detachNetworkInterface attachment Nothing
                            waitForNetworkInterfaceStatus NetworkInterfaceStatusAvailable nic
                            return inst
                    waitForInstanceState InstanceStateTerminated i
                ) `miss` anyConnectionException
  where
    req subnet = testRunInstancesRequest
        { runInstancesRequestSubnetId = Just subnet
        }

assignPrivateIpAddressesTest :: Spec
assignPrivateIpAddressesTest = do
    describe "assignPrivateIpAddresses" $ do
        context "with SecondaryPrivateIpAddressCount" $ do
            it "doesn't throw any exception" $ do
                testEC2' region (
                    withSubnet "10.0.0.0/24" $ \Subnet{subnetId = subnet} ->
                        withNetworkInterface subnet $ \NetworkInterface{networkInterfaceId = nic} ->
                            assignPrivateIpAddresses nic (Right 5) Nothing
                    ) `miss` anyConnectionException
        context "with PrivateIpAddress" $ do
            it "doesn't throw any exception" $ do
                testEC2' region (
                    withSubnet "10.0.0.0/24" $ \Subnet{subnetId = subnet} ->
                        withNetworkInterface subnet $ \NetworkInterface{networkInterfaceId = nic} ->
                            assignPrivateIpAddresses nic (Left ["10.0.0.10"]) Nothing
                    ) `miss` anyConnectionException

unassignPrivateIpAddressesTest :: Spec
unassignPrivateIpAddressesTest = do
    describe "unassignPrivateIpAddresses" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (
                withSubnet "10.0.0.0/24" $ \Subnet{subnetId = subnet} ->
                    withNetworkInterface subnet $ \NetworkInterface{networkInterfaceId = nic} -> do
                        let addr = "10.0.0.11"
                        assignPrivateIpAddresses nic (Left [addr]) Nothing
                        unassignPrivateIpAddresses nic [addr]
                ) `miss` anyConnectionException

runInstanceTest :: Spec
runInstanceTest = do
    describe "runInstances with NetworkInterfaces doesn't fail" $ do
        it "runInstances with NetworkInterfaces doesn't throw any exception" $ do
            testEC2' region test `miss` anyConnectionException
  where
    test = withSubnet "10.11.12.0/24" $ \subnet -> do
        i <- withInstance (req $ subnetId subnet) $ \i ->
            return $ instanceId i
        waitForInstanceState InstanceStateTerminated i
--        sleep 10
    req sn = testRunInstancesRequest
        { runInstancesRequestSubnetId = Nothing
        , runInstancesRequestPrivateIpAddress = Nothing
        , runInstancesRequestInstanceType = Nothing -- t1.micro causes an error "InvalidInterface.IpAddressLimitExceeded"
        , runInstancesRequestNetworkInterfaces =
            [ NetworkInterfaceParamCreate
                0
                sn
                "test"
                (Just "10.11.12.5")
                (SecondaryPrivateIpAddressParamSpecified
                    ["10.11.12.6", "10.11.12.7", "10.11.12.8"]
                    Nothing)
                []
                True
            , NetworkInterfaceParamCreate
                1
                sn
                "test"
                Nothing
                SecondaryPrivateIpAddressParamNothing
                []
                True
            ]
        }
