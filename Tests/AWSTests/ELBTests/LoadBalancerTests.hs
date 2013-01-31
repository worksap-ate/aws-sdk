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
runLoadBalancerTests = do
    hspec describeLoadBalancersTest

describeLoadBalancersTest :: Spec
describeLoadBalancersTest = do
    describe "LoadBalancer operations doesn't fail" $ do
        it "describeLoadBalancers doesn't throw any exception" $ do
            testELB region (describeLoadBalancers [] Nothing) `miss` anyHttpException
        it "createLoadBalancer and deleteLoadBalancer" $ do
            testELB region (withLoadBalancer name [listener] zones $ return ()) `miss` anyHttpException
        it "describeLoadBalancerPolicies doesn't throw any exception" $ do
            testELB region (describeLoadBalancerPolicies Nothing []) `miss` anyHttpException
        it "describeLoadBalancerPolicyTypes doesn't throw any exception" $ do
            testELB region (describeLoadBalancerPolicyTypes []) `miss` anyHttpException
        it "{create,delete}LoadBalancerPolicy doesn't throw any exception" $ do
            testELB region (withLoadBalancer name [listener] zones $ do
                let pName = "testPolicyName"
                    pTypeName = "AppCookieStickinessPolicyType"
                    attrName = "CookieName"
                    attr = PolicyAttribute attrName "testAttrValue"
                createLoadBalancerPolicy name [attr] pName pTypeName
                deleteLoadBalancerPolicy name pName
                ) `miss` anyHttpException
        it "describeInstanceHealth doesn't throw any exception" $ do
            testELB region (do
                lb:_ <- describeLoadBalancers [] Nothing
                describeInstanceHealth [] $ loadBalancerLoadBalancerName lb
                ) `miss` anyHttpException
        it "configureHealthCheck doesn't throw any exception" $ do
            testELB region (withLoadBalancer name [listener] zones $
                configureHealthCheck hc name
                ) `miss` anyHttpException
        it "{enable,disable}AvailabilityZonesForLoadBalancer doesn't throw any exception" $ do
            testELB region (withLoadBalancer name [listener] zones $ do
                let zone = "ap-northeast-1b"
                enableAvailabilityZonesForLoadBalancer [zone] name
                disableAvailabilityZonesForLoadBalancer [zone] name
                ) `miss` anyHttpException
        it "createLBCookieStickinessPolicy doesn't throw any exception" $ do
            testELB region (withLoadBalancer name [listener] zones $ do
                createLBCookieStickinessPolicy Nothing name "testCreateLBCookieStickinessPolicy1"
                createLBCookieStickinessPolicy (Just 1000) name "testCreateLBCookieStickinessPolicy2"
                ) `miss` anyHttpException
        it "createAppCookieStickinessPolicy doesn't throw any exception" $ do
            testELB region (withLoadBalancer name [listener] zones $ do
                createAppCookieStickinessPolicy "testCookieName" name "testCreateAppCookieStickinessPolicy"
                ) `miss` anyHttpException
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
