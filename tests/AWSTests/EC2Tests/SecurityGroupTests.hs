{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWSTests.EC2Tests.SecurityGroupTests
    ( runSecurityGroupTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.EC2
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runSecurityGroupTests :: IO ()
runSecurityGroupTests = do
    hspec describeSecurityGroupsTest

describeSecurityGroupsTest :: Spec
describeSecurityGroupsTest = do
    describe "describeSecurityGroups doesn't fail" $ do
        it "describeSecurityGroups doesn't throw any exception" $ do
            testEC2 region (describeSecurityGroups [] [] []) `miss` anyConnectionException
