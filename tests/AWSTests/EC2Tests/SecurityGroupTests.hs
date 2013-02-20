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
runSecurityGroupTests = hspec $ do
    describeSecurityGroupsTest
    createAndDeleteSecurityGroupTest

describeSecurityGroupsTest :: Spec
describeSecurityGroupsTest = do
    describe "describeSecurityGroups" $ do
        it "doesn't throw any exception" $ do
            testEC2 region (describeSecurityGroups [] [] []) `miss` anyConnectionException

createAndDeleteSecurityGroupTest :: Spec
createAndDeleteSecurityGroupTest = do
    describe "{create,delete}SecurityGroup" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (
                withSecurityGroup "createAndDeleteSecurityGroupTest" "For testing" Nothing $ const (return ())
                ) `miss` anyConnectionException
