module AWSTests.ELBTests.LoadBalancerTests
    ( runLoadBalancerTests
    )
    where

import Data.Text (Text)
import Test.Hspec
import qualified Control.Exception.Lifted as E

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
            testELB region (E.bracket
                (createLoadBalancer name [listener] ["ap-northeast-1a"] Nothing [] [])
                (\_ -> deleteLoadBalancer name)
                (\_ -> return ())
              ) `miss` anyHttpException
  where
    listener = Listener "http" 80 "http" Nothing 80
    name = "sdkhspectest"
