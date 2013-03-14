{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module EC2Tests.SecurityGroupTests
    ( runSecurityGroupTests
    )
    where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Test.Hspec

import Cloud.AWS.EC2
import Cloud.AWS.EC2.Types
import qualified Cloud.AWS.EC2.Util as Util
import Util
import EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runSecurityGroupTests :: IO ()
runSecurityGroupTests = hspec $ do
    describeSecurityGroupsTest
    createAndDeleteSecurityGroupTest
    authorizeAndRevokeSecurityGroupIngressTest
    authorizeAndRevokeSecurityGroupEgressTest

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

authorizeAndRevokeSecurityGroupIngressTest :: Spec
authorizeAndRevokeSecurityGroupIngressTest = do
    describe "{authorize,revoke}SecurityGroupIngress" $ do
        context "with IpRanges" $ do
            it "doesn't throw any exception" $ do
                testEC2' region (do
                    withSecurityGroup sgName "For testing" Nothing $ \msg -> do
                        let req = toReq sgName msg
                            perms = [perm1]
                        authorizeSecurityGroupIngress req perms
                        revokeSecurityGroupIngress req perms
                    ) `miss` anyConnectionException
        context "with Groups" $ do
            it "doesn't throw any exception" $ do
                testEC2' region (do
                    withSecurityGroup sgName "For testing" Nothing $ \msg -> do
                        withSecurityGroup sgName2 "For testing" Nothing $ \msg2 -> do
                            Util.sleep 5
                            let req = toReq sgName2 msg2
                                perms = [perm2 sgName msg]
                            authorizeSecurityGroupIngress req perms
                            revokeSecurityGroupIngress req perms

                        -- To test "GroupName" version
                        withSecurityGroup sgName2 "For testing" Nothing $ \_ -> do
                            let req = toReq sgName2 Nothing
                                perms = [perm2 sgName msg]
                            authorizeSecurityGroupIngress req perms
                            revokeSecurityGroupIngress req perms
                    ) `miss` anyConnectionException
  where
    toReq name = maybe (SecurityGroupRequestGroupName name) SecurityGroupRequestGroupId

    sgName = "authorizeSecurityGroupIngressTest"
    perm1 = IpPermission
        { ipPermissionIpProtocol = "tcp"
        , ipPermissionFromPort = Just 80
        , ipPermissionToPort = Just 80
        , ipPermissionGroups = []
        , ipPermissionIpRanges = ["192.0.2.0/24", "198.51.100.0/24"]
        }

    sgName2 = sgName <> "2"
    perm2 name msg = IpPermission
        { ipPermissionIpProtocol = "tcp"
        , ipPermissionFromPort = Just 80
        , ipPermissionToPort = Just 80
        , ipPermissionGroups = [toPair name msg]
        , ipPermissionIpRanges = []
        }


authorizeAndRevokeSecurityGroupEgressTest :: Spec
authorizeAndRevokeSecurityGroupEgressTest = do
    describe "authorizeSecurityGroupEgress" $ do
        context "with IpRanges" $ do
            it "doesn't throw any exception" $ do
                testEC2' region (do
                    withVpc "10.0.0.0/24" $ \Vpc{vpcId = vpc} ->
                        withSecurityGroup sgName "For testing" (Just vpc) $ \msg -> do
                            let sg = fromMaybe (error "No GroupId") msg
                                perms = [perm1]
                            authorizeSecurityGroupEgress sg perms
                            revokeSecurityGroupEgress sg perms
                    ) `miss` anyConnectionException
        context "with Groups" $ do
            it "doesn't throw any exception" $ do
                testEC2' region (do
                    withVpc "10.0.0.0/24" $ \Vpc{vpcId = vpc} ->
                        withSecurityGroup sgName "For testing" (Just vpc) $ \msg ->
                            withSecurityGroup sgName2 "For testing" (Just vpc) $ \msg2 -> do
                                let sg = fromMaybe (error "No GroupId") msg2
                                    perms = [perm2 sgName msg]
                                authorizeSecurityGroupEgress sg perms
                                revokeSecurityGroupEgress sg perms
                    ) `miss` anyConnectionException
  where
    sgName = "authorizeSecurityGroupEgressTest"
    perm1 = IpPermission
        { ipPermissionIpProtocol = "tcp"
        , ipPermissionFromPort = Just 80
        , ipPermissionToPort = Just 80
        , ipPermissionGroups = []
        , ipPermissionIpRanges = ["192.0.2.0/24", "198.51.100.0/24"]
        }

    sgName2 = sgName <> "2"
    perm2 name msg = IpPermission
        { ipPermissionIpProtocol = "tcp"
        , ipPermissionFromPort = Just 1433
        , ipPermissionToPort = Just 1433
        , ipPermissionGroups = [toPair name msg]
        , ipPermissionIpRanges = []
        }

toPair :: Text -> Maybe Text -> UserIdGroupPair
toPair _ (Just sg) = UserIdGroupPair
    { userIdGroupPairUserId = Nothing
    , userIdGroupPairGroupId = Just sg
    , userIdGroupPairGroupName = Nothing
    }
toPair name Nothing = UserIdGroupPair
    { userIdGroupPairUserId = Nothing
    , userIdGroupPairGroupId = Nothing
    , userIdGroupPairGroupName = Just name
    }
