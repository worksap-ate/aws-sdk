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
            testELB region (withLoadBalancer name [listener] $ return ()) `miss` anyHttpException
        it "describeLoadBalancerPolicies doesn't throw any exception" $ do
            testELB region (describeLoadBalancerPolicies Nothing []) `miss` anyHttpException
        it "describeLoadBalancerPolicyTypes doesn't throw any exception" $ do
            testELB region (describeLoadBalancerPolicyTypes []) `miss` anyHttpException
        it "{create,delete}LoadBalancerPolicy doesn't throw any exception" $ do
            testELB region (withLoadBalancer name [listener] $ do
                let pName = "testPolicyName"
                    pTypeName = "AppCookieStickinessPolicyType"
                    attrName = "CookieName"
                    attr = PolicyAttribute attrName "testAttrValue"
                createLoadBalancerPolicy name [attr] pName pTypeName
                deleteLoadBalancerPolicy name pName
                ) `miss` anyHttpException
  where
    listener = Listener "http" 80 "http" Nothing 80
    name = "sdkhspectest"

withLoadBalancer :: (MonadBaseControl IO m, MonadResource m) => Text -> [Listener] -> ELB m a -> ELB m a
withLoadBalancer name listeners f = E.bracket
    (createLoadBalancer name listeners ["ap-northeast-1a"] Nothing [] [])
    (const $ deleteLoadBalancer name)
    (const f)
