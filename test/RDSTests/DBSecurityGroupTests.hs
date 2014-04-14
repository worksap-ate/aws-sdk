{-# LANGUAGE FlexibleContexts #-}

module RDSTests.DBSecurityGroupTests
    ( runDBSecurityGroupTests
    )
    where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
import Data.Text (Text)
import Test.Hspec

import Cloud.AWS.EC2 (describeSecurityGroups)
import Cloud.AWS.EC2.Types (SecurityGroup(..))
import Cloud.AWS.RDS
import Cloud.AWS.RDS.Types
import Cloud.AWS.RDS.Util
import Util
import EC2Tests.Util
import RDSTests.Util

region :: Text
region = "ap-northeast-1"

runDBSecurityGroupTests :: IO ()
runDBSecurityGroupTests = do
    hspec describeDBSecurityGroupsTest
    hspec createAndDeleteDBSecurityGroupTest
    hspec authorizeRevokeDBSecurityGroupIngressTest

describeDBSecurityGroupsTest :: Spec
describeDBSecurityGroupsTest = do
    describe "describeDBSecurityGroups doesn't fail" $ do
        it "describeDBSecurityGroups doesn't throw any exception" $ do
            testRDS region (describeDBSecurityGroups Nothing Nothing Nothing)
                `miss` anyConnectionException

createAndDeleteDBSecurityGroupTest :: Spec
createAndDeleteDBSecurityGroupTest = do
    describe "{create,delete}DBSecurityGroup doesn't fail" $ do
        it "{create,delete}DBSecurityGroup doesn't throw any exception" $ do
            testRDS region (do
                createDBSecurityGroup name "test"
                deleteDBSecurityGroup name
                ) `miss` anyConnectionException
  where
    name = "hspec-test-security-group"

authorizeRevokeDBSecurityGroupIngressTest :: Spec
authorizeRevokeDBSecurityGroupIngressTest = do
    describe "{authorize,revoke}DBSecurityGroupIngress doesn't fail" $ do
        context "{authorize,revoke}DBSecurityGroupIngress doesn't throw any exception" $ do
            it "with CIDRIP" $ do
                testRDS region (do
                    name <- liftIO $ getRandomText "hspec-test-security-group-"
                    withDBSecurityGroup name $ \_ -> do
                        let ip = "10.11.12.13/32"
                        authorizeDBSecurityGroupIngress name (Just ip) Nothing Nothing Nothing
                        waitForIPRange name IPRangeStatusAuthorized
                        revokeDBSecurityGroupIngress name (Just ip) Nothing Nothing Nothing
                    ) `miss` anyConnectionException
            it "with EC2 Security Group" $ do
                sg <- head <$>
                    testEC2 region (describeSecurityGroups [] [] [])
                testRDS region (do
                    name <- liftIO $ getRandomText "hspec-test-security-group-"
                    withDBSecurityGroup name $ \_ -> do
                        authorizeDBSecurityGroupIngress name Nothing
                            (Just $ securityGroupId sg)
                            Nothing
                            (Just $ securityGroupOwnerId sg)
                        waitForEC2SecurityGroup name EC2SecurityGroupStatusAuthorized
                        revokeDBSecurityGroupIngress name Nothing
                            (Just $ securityGroupId sg)
                            Nothing
                            (Just $ securityGroupOwnerId sg)
                    ) `miss` anyConnectionException

waitForIPRange
    :: (MonadBaseControl IO m, MonadResource m)
    => Text
    -> IPRangeStatus
    -> RDS m DBSecurityGroup
waitForIPRange name status = wait
    (\dbsg -> all (== status) $
        map ipRangeStatus $
        dbSecurityGroupIPRanges dbsg)
    (\dbsgid -> describeDBSecurityGroups (Just dbsgid) Nothing Nothing)
    name

waitForEC2SecurityGroup
    :: (MonadBaseControl IO m, MonadResource m)
    => Text
    -> EC2SecurityGroupStatus
    -> RDS m DBSecurityGroup
waitForEC2SecurityGroup name status = wait
    (\dbsg -> all (== status) $
        map ec2SecurityGroupStatus $
        dbSecurityGroupEC2SecurityGroups dbsg)
    (\dbsgid -> describeDBSecurityGroups (Just dbsgid) Nothing Nothing)
    name
