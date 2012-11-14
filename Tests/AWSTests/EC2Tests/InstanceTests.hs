{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWSTests.EC2Tests.InstanceTests
    ( runInstanceTests
    )
    where

import Data.Text (Text)
import Data.List
import Test.Hspec

import AWS.EC2
import AWS.EC2.Types
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runInstanceTests :: IO ()
runInstanceTests = do
    hspec describeInstancesTest
    hspec describeInstanceStatusTest
    hspec describeInstanceAttributeTest

describeInstancesTest :: Spec
describeInstancesTest = do
    describe "describeInstances doesn't fail" $ do
        it "describeInstances doesn't throw any exception" $ do
            testEC2 region (describeInstances [] []) `miss` anyHttpException

describeInstanceStatusTest :: Spec
describeInstanceStatusTest = do
    describe "describeInstanceStatus doesn't fail" $ do
        it "describeInstanceStatus doesn't throw any exception" $ do
            reservations <- testEC2 region (describeInstances [] [])
            let instances = nub $ concat $ map instanceSet reservations
            testEC2 region (describeInstanceStatus (map instanceId instances) True [] Nothing) `miss` anyHttpException

describeInstanceAttributeTest :: Spec
describeInstanceAttributeTest = do
    describe "describeInstanceAttribute doesn't fail" $ do
        it "describeInstanceAttribute doesn't throw any exception" $ do
            reservations <- testEC2 region (describeInstances [] [])
            let iid = instanceId $ head $ instanceSet $ head reservations
            testEC2' region (describeInstanceAttribute iid IARInstanceType) `miss` anyHttpException
