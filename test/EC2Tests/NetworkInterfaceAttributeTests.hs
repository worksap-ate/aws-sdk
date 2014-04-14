{-# LANGUAGE FlexibleContexts #-}
module EC2Tests.NetworkInterfaceAttributeTests
    ( runNetworkInterfaceAttributeTests
    ) where

import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import Data.Conduit (($$+-))
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
import Test.Hspec

import Cloud.AWS.EC2
import Cloud.AWS.EC2.Types
import Util
import EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runNetworkInterfaceAttributeTests :: IO ()
runNetworkInterfaceAttributeTests = hspec $ do
    describeNetworkInterfaceAttributeTest
    modifyNetworkInterfaceAttributeTest
    resetNetworkInterfaceAttributeTest

describeNetworkInterfaceAttributeTest :: Spec
describeNetworkInterfaceAttributeTest = do
    describe "DescribeNetworkInterfaceAttribute" $ do
        describe "Description" $ do
            it "doesn't throw any exception" $ do
                testEC2' region (getNetworkInterfaceId >>= describeNetworkInterfaceDescription)
                    `miss` anyConnectionException
        describe "GroupSet" $ do
            it "doesn't throw any exception" $ do
                testEC2' region (getNetworkInterfaceId >>= describeNetworkInterfaceGroupSet)
                    `miss` anyConnectionException
        describe "SourceDestCheck" $ do
            it "doesn't throw any exception" $ do
                testEC2' region (getNetworkInterfaceId >>= describeNetworkInterfaceSourceDestCheck)
                    `miss` anyConnectionException
        describe "Attachment" $ do
            it "doesn't throw any exception" $ do
                testEC2' region (getNetworkInterfaceId >>= describeNetworkInterfaceAttachment)
                    `miss` anyConnectionException

resetNetworkInterfaceAttributeTest :: Spec
resetNetworkInterfaceAttributeTest = do
    describe "ResetNetworkInterfaceAttribute" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (getNetworkInterfaceId >>= resetNetworkInterfaceSourceDestCheck)
                `miss` anyConnectionException

getNetworkInterfaceId :: (MonadBaseControl IO m, MonadResource m) => EC2 m Text
getNetworkInterfaceId = do
    ifacesSrc <- describeNetworkInterfaces [] []
    Just iface <- lift $ ifacesSrc $$+- CL.head
    return $ networkInterfaceId iface

modifyNetworkInterfaceAttributeTest :: Spec
modifyNetworkInterfaceAttributeTest = do
    describe "ModifyNetworkInterfaceAttribute" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (do
                withSubnet "10.0.0.0/24" $ \Subnet{subnetId = subnet, subnetVpcId = vpc} -> do
                    withNetworkInterface subnet $ \NetworkInterface{networkInterfaceId = nic, networkInterfaceGroupSet = sgs} -> do
                        i <- withInstance (testRunInstancesRequest{runInstancesRequestSubnetId = Just subnet}) $ \Instance{instanceId = inst} -> do
                            waitForInstanceState InstanceStateRunning inst
                            attachment <- attachNetworkInterface nic inst 1
                            modifyNetworkInterfaceAttachment nic attachment False
                            detachNetworkInterface attachment Nothing
                            waitForNetworkInterfaceStatus NetworkInterfaceStatusAvailable nic
                            return inst
                        withSecurityGroup "modifyNetworkInterfaceAttributeTest" "For testing" (Just vpc) $ \msg -> do
                            modifyNetworkInterfaceSecurityGroup nic [fromMaybe (error "No GroupId") msg]
                            -- Reset SecurityGroups.
                            -- Without this, DependencyViolation is raised because the NetworkInterface depends on the created SecurityGroup.
                            modifyNetworkInterfaceSecurityGroup nic $ catMaybes $ map groupId sgs
                        modifyNetworkInterfaceDescription nic "Test Description"
                        modifyNetworkInterfaceSourceDestCheck nic True

                        waitForInstanceState InstanceStateTerminated i
                ) `miss` anyConnectionException
