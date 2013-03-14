{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module EC2Tests.InstanceTests
    ( runInstanceTests
    )
    where

import Control.Applicative ((<$>))
import Data.Text (Text)
import Data.List
import Test.Hspec

import Cloud.AWS.EC2
import qualified Cloud.AWS.EC2.Util as Util
import Cloud.AWS.EC2.Types
import Util
import EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runInstanceTests :: IO ()
runInstanceTests = hspec $ do
    describeInstancesTest
    describeInstanceStatusTest
    describeInstanceAttributeTest
    monitorAndUnmonitorInstancesTest
    runRebootTerminateInstanceTest
    startStopInstanceTest
    getConsoleOutputTest

describeInstancesTest :: Spec
describeInstancesTest = do
    describe "describeInstances doesn't fail" $ do
        it "describeInstances doesn't throw any exception" $ do
            testEC2 region (describeInstances [] []) `miss` anyConnectionException

describeInstanceStatusTest :: Spec
describeInstanceStatusTest = do
    describe "describeInstanceStatus doesn't fail" $ do
        it "describeInstanceStatus doesn't throw any exception" $ do
            testEC2 region (do
                reservations <- Util.list $ describeInstances [] []
                let instances = take 100 $ nub $ concat $ map reservationInstanceSet reservations
                describeInstanceStatus (map instanceId instances) True [] Nothing
              ) `miss` anyConnectionException

describeInstanceAttributeTest :: Spec
describeInstanceAttributeTest = do
    describe "describeInstanceAttribute doesn't fail" $ do
        it "describeInstanceAttribute doesn't throw any exception" $ do
            testEC2' region (do
                reservations <- Util.list $ describeInstances [] []
                let iid = instanceId $ head $ reservationInstanceSet $ head reservations
                describeInstanceAttribute iid InstanceAttributeRequestInstanceType 
              ) `miss` anyConnectionException

monitorAndUnmonitorInstancesTest :: Spec
monitorAndUnmonitorInstancesTest = do
    describe "{monitor,unmonitor}Instances doesn't fail" $ do
        it "{monitor,unmonitor}Instances doesn't throw any exception" $ (do
            reservations <- testEC2 region $ describeInstances [] [("monitoring-state", ["disabled"])]
            let iid = instanceId $ head $ reservationInstanceSet $ head reservations
            testState (monitorInstances [iid]) >>= (`shouldSatisfy` (`elem` [MonitoringPending, MonitoringEnabled]))
            testState (unmonitorInstances [iid]) >>= (`shouldSatisfy` (`elem` [MonitoringDisabling, MonitoringDisabled]))
            ) `miss` anyConnectionException
  where
    testState ec2 =
        monitorInstancesResponseInstanceMonitoringState . head
        <$> testEC2 region ec2

runRebootTerminateInstanceTest :: Spec
runRebootTerminateInstanceTest = do
    describe "{run,reboot,terminate}Instances" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (
                withInstance testRunInstancesRequest $ \Instance{instanceId = inst} ->
                    rebootInstances [inst]
                ) `miss` anyConnectionException

startStopInstanceTest :: Spec
startStopInstanceTest = do
    describe "{start,stop}Instances and {modify,reset}InstanceAttribute" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (
                withInstance testRunInstancesRequest $ \Instance{instanceId = inst} -> do
                    waitForInstanceState InstanceStateRunning inst
                    stopInstances [inst] True
                    waitForInstanceState InstanceStateStopped inst

                    mapM_ (modifyInstanceAttribute inst)
                        [ModifyInstanceAttributeRequestInstanceType "m1.small"
                        , ModifyInstanceAttributeRequestUserData "user data"
                        , ModifyInstanceAttributeRequestDisableApiTermination False
                        , ModifyInstanceAttributeRequestShutdownBehavior ShutdownBehaviorTerminate
                        ]

                    mapM_ (resetInstanceAttribute inst)
                        [ ResetInstanceAttributeRequestKernel
                        , ResetInstanceAttributeRequestRamdisk
                        ]

                    startInstances [inst]
                    waitForInstanceState InstanceStateRunning inst
                ) `miss` anyConnectionException

getConsoleOutputTest :: Spec
getConsoleOutputTest = do
    describe "getConsoleOutput" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (do
                Just Reservation{reservationInstanceSet = Instance{instanceId = inst}:_}
                    <- Util.head $ describeInstances [] []
                getConsoleOutput inst
                ) `miss` anyConnectionException
