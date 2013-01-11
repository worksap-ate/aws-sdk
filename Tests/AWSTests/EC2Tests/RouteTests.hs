{-# LANGUAGE FlexibleContexts #-}

module AWSTests.EC2Tests.RouteTests
    ( runRouteTests
    )
    where

import Data.IP (AddrRange, IPv4)
import Data.Text (Text)
import Data.Conduit (($$+-))
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Class (lift)
import Test.Hspec

import AWS.EC2
import AWS.EC2.Types (RouteTable(..), InternetGateway(..), CreateRouteRequest(..))
import AWSTests.Util
import AWSTests.EC2Tests.Util

region :: Text
region = "ap-northeast-1"

cidr :: AddrRange IPv4
cidr = "10.0.0.0/8"

runRouteTests :: IO ()
runRouteTests = do
    hspec routeTest

routeTest :: Spec
routeTest = do
    describe "{create,replace,delete}Route doesn't fail" $ do
        it "{create,replace,delete}Route doesn't throw any exception" $ do
            testEC2' region (do
                tablesSrc <- describeRouteTables [] []
                Just table <- lift $ tablesSrc $$+- CL.head
                let tableId = routeTableId table
                gatewaysSrc <- describeInternetGateways [] []
                Just gateway <- lift $ gatewaysSrc $$+- CL.head
                let gatewayId = internetGatewayInternetGatewayId gateway
                createRoute $ CreateRouteToGateway tableId cidr gatewayId
                replaceRoute $ CreateRouteToGateway tableId cidr gatewayId
                deleteRoute tableId cidr
                ) `miss` anyHttpException
