{-# LANGUAGE FlexibleContexts #-}
module AWSTests.ELBTests.LoadBalancerTests
    ( runLoadBalancerTests
    )
    where

import Data.Text (Text)
import Test.Hspec
import qualified Control.Exception.Lifted as E
import Data.Conduit (MonadBaseControl, MonadResource)

import AWS.ELB
import AWS.ELB.Types
import AWSTests.Util
import AWSTests.ELBTests.Util

region :: Text
region = "ap-northeast-1"

runLoadBalancerTests :: IO ()
runLoadBalancerTests = hspec $ do
    describe "describeLoadBalancers" $ do
        it "doesn't throw any exception" $ do
            testELB region (describeLoadBalancers [] Nothing) `miss` anyConnectionException

    describe "{create,delete}LoadBalancer" $ do
        it "doesn't throw any exception" $ do
            testELB region (withLoadBalancer name [listener] zones $ return ()) `miss` anyConnectionException

    describe "describeLoadBalancerPolicies" $ do
        it "doesn't throw any exception" $ do
            testELB region (describeLoadBalancerPolicies Nothing []) `miss` anyConnectionException

    describe "describeLoadBalancerPolicyTypes" $ do
        it "doesn't throw any exception" $ do
            testELB region (describeLoadBalancerPolicyTypes []) `miss` anyConnectionException

    describe "{create,delete}LoadBalancerPolicy" $ do
        it "doesn't throw any exception" $ do
            testELB region (withLoadBalancer name [listener] zones $ do
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
            testELB region (withLoadBalancer name [listener] zones $
                configureHealthCheck hc name
                ) `miss` anyConnectionException

    describe "{enable,disable}AvailabilityZonesForLoadBalancer" $ do
        it "doesn't throw any exception" $ do
            testELB region (withLoadBalancer name [listener] zones $ do
                let zone = "ap-northeast-1b"
                enableAvailabilityZonesForLoadBalancer [zone] name
                disableAvailabilityZonesForLoadBalancer [zone] name
                ) `miss` anyConnectionException

    describe "createLBCookieStickinessPolicy" $ do
        it "doesn't throw any exception" $ do
            testELB region (withLoadBalancer name [listener] zones $ do
                createLBCookieStickinessPolicy Nothing name "testCreateLBCookieStickinessPolicy1"
                createLBCookieStickinessPolicy (Just 1000) name "testCreateLBCookieStickinessPolicy2"
                ) `miss` anyConnectionException

    describe "createAppCookieStickinessPolicy" $ do
        it "doesn't throw any exception" $ do
            testELB region (withLoadBalancer name [listener] zones $ do
                createAppCookieStickinessPolicy "testCookieName" name "testCreateAppCookieStickinessPolicy"
                ) `miss` anyConnectionException

    describe "setLoadBalancerPoliciesOfListener" $ do
        it "doesn't throw any exception" $ do
            testELB region (withLoadBalancer name [listener] zones $ do
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

withLoadBalancer :: (MonadBaseControl IO m, MonadResource m) => Text -> [Listener] -> [Text] -> ELB m a -> ELB m a
withLoadBalancer name listeners zones f = E.bracket
    (createLoadBalancer name listeners zones Nothing [] [])
    (const $ deleteLoadBalancer name)
    (const f)
