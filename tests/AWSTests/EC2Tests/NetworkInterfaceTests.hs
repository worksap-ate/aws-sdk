{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWSTests.EC2Tests.NetworkInterfaceTests
    ( runNetworkInterfaceTests
    ) where

import Data.Text (Text)
import Data.Conduit (MonadBaseControl, MonadResource)
import Test.Hspec

import AWS.EC2
import AWS.EC2.Types
import qualified AWS.EC2.Util as Util
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runNetworkInterfaceTests :: IO ()
runNetworkInterfaceTests = hspec $ do
    describeNetworkInterfacesTest
    createAndDeleteNetworkInterfaceTest
    attachAndDetachNetworkInterfaceTest
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

request :: RunInstancesRequest
request = defaultRunInstancesRequest "ami-087acb09" 1 1

waitForInstanceState :: (MonadBaseControl IO m, MonadResource m) => InstanceState -> Text -> EC2 m Reservation
waitForInstanceState s = Util.wait p desc
  where
    p r = (instanceState . head . reservationInstanceSet) r == s
    desc inst = Util.list $ describeInstances [inst] []

waitForNetworkInterfaceStatus :: (MonadBaseControl IO m, MonadResource m) => NetworkInterfaceStatus -> Text -> EC2 m NetworkInterface
waitForNetworkInterfaceStatus s = Util.wait p desc
  where
    p r = networkInterfaceStatus r == s
    desc nic = Util.list $ describeNetworkInterfaces [nic] []

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
    req subnet = request
        { runInstancesRequestSubnetId = Just subnet
        }

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
    req sn = request
        { runInstancesRequestSubnetId = Nothing
        , runInstancesRequestPrivateIpAddress = Nothing
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
