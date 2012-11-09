{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.EC2.InstanceTests
    ( runInstanceTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.EC2
import AWS.Util
import AWS.EC2.Util
import Control.Exception

region :: Text
region = "ap-northeast-1"

runInstanceTests :: IO ()
runInstanceTests = do
    hspec describeInstancesTest

describeInstancesTest :: Spec
describeInstancesTest = do
    describe "describeInstances doesn't fail" $ do
        it "describeInstances doesn't throw any exception" $ do
            testEC2 region (describeInstances [] []) `shouldntThrow` anyException
