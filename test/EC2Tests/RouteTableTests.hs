{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module EC2Tests.RouteTableTests
    ( runRouteTableTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import Cloud.AWS.EC2
import Cloud.AWS.EC2.Types
import Util
import EC2Tests.Util

region :: Text
region = "ap-northeast-1"

runRouteTableTests :: IO ()
runRouteTableTests = hspec $ do
    describeRouteTablesTest
    createAndDeleteRouteTableTest
    associateAndDisassociateRouteTableTest

describeRouteTablesTest :: Spec
describeRouteTablesTest = do
    describe "describeRouteTables" $ do
        it "doesn't throw any exception" $ do
            testEC2 region (describeRouteTables [] []) `miss` anyConnectionException

createAndDeleteRouteTableTest :: Spec
createAndDeleteRouteTableTest = do
    describe "{create,delete}RouteTable" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (
                withVpc "10.0.0.0/24" $ \Vpc{vpcId = vpc} ->
                    withRouteTable vpc $ const (return ())
                ) `miss` anyConnectionException

associateAndDisassociateRouteTableTest :: Spec
associateAndDisassociateRouteTableTest = do
    describe "[dis]associateRouteTable" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (
                withSubnet "10.0.0.0/24" $ \Subnet{subnetId = subnet, subnetVpcId = vpc} ->
                    withRouteTable vpc $ \RouteTable{routeTableId = routeTable} -> do
                        association <- associateRouteTable routeTable subnet
                        disassociateRouteTable association
                ) `miss` anyConnectionException
