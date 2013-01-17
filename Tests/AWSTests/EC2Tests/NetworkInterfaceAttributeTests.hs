{-# LANGUAGE FlexibleContexts #-}
module AWSTests.EC2Tests.NetworkInterfaceAttributeTests
    ( runNetworkInterfaceAttributeTests
    ) where

import Data.Text (Text)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Conduit (($$+-), MonadResource)
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Class (lift)
import Test.Hspec

import AWS.EC2
import AWS.EC2.Types (NetworkInterface(..))
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runNetworkInterfaceAttributeTests :: IO ()
runNetworkInterfaceAttributeTests = do
    hspec describeNetworkInterfaceAttributeTest
    hspec resetNetworkInterfaceAttributeTest

describeNetworkInterfaceAttributeTest :: Spec
describeNetworkInterfaceAttributeTest = do
    describe "DescribeNetworkInterfaceAttribute doesn't fail" $ do
        it "describeNetworkInterfaceDescription doesn't throw any exception" $ do
            testEC2' region (getNetworkInterfaceId >>= describeNetworkInterfaceDescription)
                `miss` anyHttpException
        it "describeNetworkInterfaceGroupSet doesn't throw any exception" $ do
            testEC2' region (getNetworkInterfaceId >>= describeNetworkInterfaceGroupSet)
                `miss` anyHttpException
        it "describeNetworkInterfaceSourceDestCheck doesn't throw any exception" $ do
            testEC2' region (getNetworkInterfaceId >>= describeNetworkInterfaceSourceDestCheck)
                `miss` anyHttpException
        it "describeNetworkInterfaceAttachment doesn't throw any exception" $ do
            testEC2' region (getNetworkInterfaceId >>= describeNetworkInterfaceAttachment)
                `miss` anyHttpException

resetNetworkInterfaceAttributeTest :: Spec
resetNetworkInterfaceAttributeTest = do
    describe "ResetNetworkInterfaceAttribute doesn't fail" $ do
        it "resetNetworkInterfaceSourceDestCheck doesn't throw any exception" $ do
            testEC2' region (getNetworkInterfaceId >>= resetNetworkInterfaceSourceDestCheck)
                `miss` anyHttpException

getNetworkInterfaceId :: (MonadBaseControl IO m, MonadResource m) => EC2 m Text
getNetworkInterfaceId = do
    ifacesSrc <- describeNetworkInterfaces [] []
    Just iface <- lift $ ifacesSrc $$+- CL.head
    return $ networkInterfaceId iface
