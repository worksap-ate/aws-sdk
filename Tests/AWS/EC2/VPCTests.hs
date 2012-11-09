{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.EC2.VPCTests
    ( runVpcTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.EC2
import AWS.EC2.Types
import AWS.Util
import AWS.EC2.Util

region :: Text
region = "ap-southeast-1"

runVpcTests :: IO ()
runVpcTests = do
    hspec describeVpcsTest
    hspec createVpcTest

describeVpcsTest :: Spec
describeVpcsTest = do
    describe "describeVpcs doesn't fail" $ do
        it "describeVpcs doesn't throw any exception" $ do
            testEC2 region (describeVpcs [] []) `shouldntThrow` anyException

createVpcTest :: Spec
createVpcTest = do
    describe "createVpc doesn't fail" $ do
        it "createVpc and deleteVpc doesn't fail" $ do
            vpc <- testEC2' region (createVpc "80.0.0.0/16" Nothing)
            testEC2' region (deleteVpc $ vpcId vpc) `shouldReturn` True
