{-# LANGUAGE FlexibleContexts #-}
module ELBTests.LoadBalancerTests
    ( runLoadBalancerTests
    )
    where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadBaseControl, MonadResource)
import Data.IP (AddrRange, IPv4)
import Data.Text (Text)
import Test.Hspec
import qualified Control.Exception.Lifted as E

import Cloud.AWS.EC2.Types (Vpc(..), Subnet(..), InternetGateway(..), Instance(..))
import Cloud.AWS.ELB
import Cloud.AWS.ELB.Types hiding (Instance(..))
import Util
import EC2Tests.Util
    ( testEC2'
    , withVpc
    , withSubnet'
    , withInternetGateway
    , withInternetGatewayAttached
    , withSecurityGroup
    , withInstance
    , testRunInstancesRequest
    )
import ELBTests.Util

region :: Text
region = "ap-northeast-1"

runLoadBalancerTests :: IO ()
runLoadBalancerTests = hspec $ do
    describe "describeLoadBalancers" $ do
        it "doesn't throw any exception" $ do
            testELB region (describeLoadBalancers [] Nothing) `miss` anyConnectionException

    describe "{create,delete}LoadBalancer" $ do
        context "EC2" $ do
            it "doesn't throw any exception" $ do
                testELB region (withLoadBalancer name [listener] zones [] [] $ return ()) `miss` anyConnectionException
        context "with VPC" $ do
            it "doesn't throw any exception" $ do
                withSubnets "10.0.0.0/24" [("10.0.0.0/24", Nothing)] $ \_ _ subnets ->
                    testELB region (withLoadBalancer name [listener] [] [] subnets $ return ()) `miss` anyConnectionException

    describe "attachLoadBalancerToSubnets and detachLoadBalancerFromSubnets" $ do
        it "doesn't throw any exception" $ (do
            withSubnets "10.0.0.0/16" [("10.0.0.0/24", Just "ap-northeast-1a"), ("10.0.1.0/24", Just "ap-northeast-1b")] $ \_ _ [sub1, sub2] ->
                testELB region (withLoadBalancer name [listener] [] [] [sub1] $ do
                    attachLoadBalancerToSubnets name [sub2]
                    detachLoadBalancerFromSubnets name [sub1]
                    )
            ) `miss` anyConnectionException

    describe "applySecurityGroupsToLoadBalancer" $ do
        it "doesn't throw any exception" $ (do
            withSubnets "10.0.0.0/24" [("10.0.0.0/24", Nothing)] $ \vpc _ subnets -> do
                let sgName1 = "applySecurityGroupsToLoadBalancerTest1"
                    sgName2 = "applySecurityGroupsToLoadBalancerTest2"
                testEC2' region (withSecurityGroup sgName1 "For HSpec testing" (Just vpc) $ \(Just sg1) ->
                    withSecurityGroup sgName2 "For HSpec testing" (Just vpc) $ \(Just sg2) -> liftIO $
                        testELB region (withLoadBalancer name [listener] [] [sg1] subnets $
                            applySecurityGroupsToLoadBalancer name [sg2]
                            )
                    )
            ) `miss` anyConnectionException

    describe "registerInstancesWithLoadBalancer and deregisterInstancesFromLoadBalancer" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (withInstance testRunInstancesRequest $ \Instance{instanceId = inst} -> liftIO $
                testELB region (withLoadBalancer name [listener] zones [] [] $ do
                    registerInstancesWithLoadBalancer [inst] name
                    deregisterInstancesFromLoadBalancer [inst] name
                    )
                ) `miss` anyConnectionException

    describe "{create,delete}LoadBalancerListeners" $ do
        it "doesn't throw any exception" $ do
            testELB region (withLoadBalancer name [listener] zones [] [] $ do
                let listener2 = Listener "http" 8080 "http" Nothing 80
                createLoadBalancerListeners [listener2] name
                deleteLoadBalancerListeners name [listenerLoadBalancerPort listener2]
                ) `miss` anyConnectionException

    describe "describeLoadBalancerPolicies" $ do
        it "doesn't throw any exception" $ do
            testELB region (describeLoadBalancerPolicies Nothing []) `miss` anyConnectionException

    describe "describeLoadBalancerPolicyTypes" $ do
        it "doesn't throw any exception" $ do
            testELB region (describeLoadBalancerPolicyTypes []) `miss` anyConnectionException

    describe "{create,delete}LoadBalancerPolicy" $ do
        it "doesn't throw any exception" $ do
            testELB region (withLoadBalancer name [listener] zones [] [] $ do
                let pName = "testPolicyName"
                    pTypeName = "AppCookieStickinessPolicyType"
                    attrName = "CookieName"
                    attr = PolicyAttribute attrName "testAttrValue"
                createLoadBalancerPolicy name [attr] pName pTypeName
                deleteLoadBalancerPolicy name pName
                ) `miss` anyConnectionException

    describe "describeInstanceHealth" $ do
        it "doesn't throw any exception" $ do
            testELB region (do
                lb:_ <- describeLoadBalancers [] Nothing
                describeInstanceHealth [] $ loadBalancerLoadBalancerName lb
                ) `miss` anyConnectionException

    describe "configureHealthCheck" $ do
        it "doesn't throw any exception" $ do
            testELB region (withLoadBalancer name [listener] zones [] [] $
                configureHealthCheck hc name
                ) `miss` anyConnectionException

    describe "{enable,disable}AvailabilityZonesForLoadBalancer" $ do
        it "doesn't throw any exception" $ do
            testELB region (withLoadBalancer name [listener] zones [] [] $ do
                let zone = "ap-northeast-1b"
                enableAvailabilityZonesForLoadBalancer [zone] name
                disableAvailabilityZonesForLoadBalancer [zone] name
                ) `miss` anyConnectionException

    describe "createLBCookieStickinessPolicy" $ do
        it "doesn't throw any exception" $ do
            testELB region (withLoadBalancer name [listener] zones [] [] $ do
                createLBCookieStickinessPolicy Nothing name "testCreateLBCookieStickinessPolicy1"
                createLBCookieStickinessPolicy (Just 1000) name "testCreateLBCookieStickinessPolicy2"
                ) `miss` anyConnectionException

    describe "createAppCookieStickinessPolicy" $ do
        it "doesn't throw any exception" $ do
            testELB region (withLoadBalancer name [listener] zones [] [] $ do
                createAppCookieStickinessPolicy "testCookieName" name "testCreateAppCookieStickinessPolicy"
                ) `miss` anyConnectionException

    describe "setLoadBalancerPoliciesOfListener" $ do
        it "doesn't throw any exception" $ do
            testELB region (withLoadBalancer name [listener] zones [] [] $ do
                let policy = "setLoadBalancerPoliciesOfListener"
                createLBCookieStickinessPolicy Nothing name policy
                setLoadBalancerPoliciesOfListener name (listenerLoadBalancerPort listener) [policy]
                setLoadBalancerPoliciesOfListener name (listenerLoadBalancerPort listener) []
                ) `miss` anyConnectionException
  where
    listener = Listener "http" 80 "http" Nothing 80
    name = "sdkhspectest"
    zones = ["ap-northeast-1a"]
    hc = HealthCheck
        { healthCheckHealthyThreshold = 10
        , healthCheckInterval = 60
        , healthCheckTimeout = 3
        , healthCheckTarget = "TCP:80"
        , healthCheckUnhealthyThreshold = 3
        }

withLoadBalancer :: (MonadBaseControl IO m, MonadResource m) => Text -> [Listener] -> [Text] -> [Text] -> [Text] -> ELB m a -> ELB m a
withLoadBalancer name listeners zones sgs subnets f = E.bracket
    (createLoadBalancer name listeners zones Nothing sgs subnets)
    (const $ deleteLoadBalancer name)
    (const f)

withSubnets :: AddrRange IPv4 -> [(AddrRange IPv4, Maybe Text)] -> (Text -> Text -> [Text] -> IO a) -> IO a
withSubnets vpcCidr reqs f = testEC2' region $
    withVpc vpcCidr $ \Vpc{vpcId = vpc} ->
        withInternetGateway $ \InternetGateway{internetGatewayInternetGatewayId = igw} ->
            withInternetGatewayAttached igw vpc $
                go vpc igw reqs []
  where
    go vpc igw [] ids = liftIO $ f vpc igw ids
    go vpc igw ((cidr,zone):xs) ids = withSubnet' vpc cidr zone $ \Subnet{subnetId = subnet} -> go vpc igw xs (subnet:ids)
