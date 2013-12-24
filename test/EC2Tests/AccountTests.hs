{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module EC2Tests.AccountTests
    ( runAccountTests
    ) where

import Data.Text (Text)
import Test.Hspec

import Cloud.AWS.EC2
import Cloud.AWS.EC2.Types
import EC2Tests.Util
import Util

region :: Text
region = "ap-northeast-1"

runAccountTests :: IO ()
runAccountTests = hspec $ do
    describe "describeAccountAttributes" $ do
        it "doesn't throw any exception" $ do
            testEC2 region (do
                describeAccountAttributes []
                describeAccountAttributes allAttrNames
                ) `miss` anyConnectionException
  where
    allAttrNames =
        [ AttributeNameSupportedPlatforms
        , AttributeNameDefaultVpc
        , AttributeNameVpcMaxSecurityGroupsPerInterface
        , AttributeNameMaxInstances
        , AttributeNameMaxElasticIPs
        , AttributeNameVpcMaxElasticIPs
        ]
