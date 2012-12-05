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

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative

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
    test = do
        vpc <- createVpc cidr Nothing
        liftIO $ threadDelay $ 2 * 1000 * 1000
        subnet <- createSubnet $ CreateSubnetRequest (vpcId vpc) cidr Nothing
        liftIO $ threadDelay $ 2 * 1000 * 1000
        i <- head . reservationInstanceSet <$> runInstances (req $ subnetId subnet)
        liftIO $ threadDelay $ 2 * 1000 * 1000
        terminateInstances [instanceId i]
        Util.wait
            (\r -> (instanceState . head . reservationInstanceSet) r == InstanceStateTerminated)
            (\iid -> Util.list (describeInstances [iid] []))
            (instanceId i)
        liftIO $ threadDelay $ 10 * 1000 * 1000
        deleteSubnet $ subnetId subnet
        deleteVpc $ vpcId vpc
        return ()
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
    cidr = "10.11.12.0/24"
