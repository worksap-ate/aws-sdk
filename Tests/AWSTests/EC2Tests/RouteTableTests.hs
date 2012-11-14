{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWSTests.EC2Tests.RouteTableTests
    ( runRouteTableTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.EC2
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runRouteTableTests :: IO ()
runRouteTableTests = do
    hspec describeRouteTablesTest

describeRouteTablesTest :: Spec
describeRouteTablesTest = do
    describe "describeRouteTables doesn't fail" $ do
        it "describeRouteTables doesn't throw any exception" $ do
            testEC2 region (describeRouteTables [] []) `miss` anyHttpException
