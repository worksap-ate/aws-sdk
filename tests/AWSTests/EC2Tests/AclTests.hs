module AWSTests.EC2Tests.AclTests (runAclTests) where

import Data.Text (Text)
import Test.Hspec

import AWS.EC2
import AWS.EC2.Types
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-southeast-1"

runAclTests :: IO ()
runAclTests = hspec $ do
    describe "describeNetworkAcls" $ do
        it "doesn't throw any exception" $ do
            testEC2 region (describeNetworkAcls [] []) `miss` anyHttpException

    describe "{create,delete}NetworkAcl" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (do
                withVpc "10.0.0.0/24" $ \Vpc{vpcId = vpc} ->
                    withNetworkAcl vpc $ const (return ())
                ) `miss` anyHttpException
