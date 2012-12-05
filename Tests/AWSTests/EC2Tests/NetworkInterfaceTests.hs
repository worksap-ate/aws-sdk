{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWSTests.EC2Tests.NetworkInterfaceTests
    ( runNetworkInterfaceTests
    ) where

import Data.Text (Text)
import Test.Hspec

import AWS.EC2
import AWS.EC2.Types
import qualified AWS.EC2.Util as Util
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runNetworkInterfaceTests :: IO ()
runNetworkInterfaceTests = do
    hspec describeNetworkInterfacesTest
    hspec runInstanceTest

describeNetworkInterfacesTest :: Spec
describeNetworkInterfacesTest = do
    describe "describeNetworkInterfaces doesn't fail" $ do
        it "describeNetworkInterfaces doesn't throw any exception" $ do
            testEC2 region (describeNetworkInterfaces [] []) `miss` anyHttpException

runInstanceTest :: Spec
runInstanceTest = do
    describe "runInstances with NetworkInterfaces doesn't fail" $ do
        it "runInstances with NetworkInterfaces doesn't throw any exception" $ do
            testEC2' region test `miss` anyHttpException
  where
    test = withSubnet "10.11.12.0/24" $ \subnet -> do
        i <- withInstance (req $ subnetId subnet) $ \i ->
            return $ instanceId i
        Util.wait
            (\r -> (instanceState . head . reservationInstanceSet) r == InstanceStateTerminated)
            (\iid -> Util.list (describeInstances [iid] []))
            i
        sleep 10
    req sn = (defaultRunInstancesRequest "ami-087acb09" 1 1)
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
