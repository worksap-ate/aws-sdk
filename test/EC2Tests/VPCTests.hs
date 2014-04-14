{-# LANGUAGE FlexibleContexts, RankNTypes, PatternGuards, TupleSections #-}

module EC2Tests.VPCTests
    ( runVpcTests
    )
    where

import Control.Applicative ((<$>))
import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
import Data.Text (Text)
import Data.List (find)
import Test.Hspec

import Cloud.AWS.EC2
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Util (wait, list)
import Util
import EC2Tests.Util

region :: Text
region = "ap-southeast-1"

runVpcTests :: IO ()
runVpcTests = hspec $ do
    describeVpcsTest
    describeVpnGatewaysTest
    describeInternetGatewaysTest
    describeVpnConnectionsTest
    describeCustomerGatewayTest
    describeInternetGatewaysTest
    describeDhcpOptionsTest
    createVpcTest
    createDhcpOptionsTest
    vpnConnectionTest
    attachAndDetachVpnGatewayTest
    internetGatewayTest
    associateDhcpOptionsTest
    createAndDeleteVpnConnectionRouteTest
    enableAndDisableVgwRoutePropagationTest

describeVpcsTest :: Spec
describeVpcsTest = do
    describe "describeVpcs doesn't fail" $ do
        it "describeVpcs doesn't throw any exception" $ do
            testEC2 region (describeVpcs [] []) `miss` anyConnectionException

describeVpnGatewaysTest :: Spec
describeVpnGatewaysTest = do
    describe "describeVpnGateways doesn't fail" $ do
        it "describeVpnGateways doesn't throw any exception" $ do
            testEC2 region (describeVpnGateways [] []) `miss` anyConnectionException

describeVpnConnectionsTest :: Spec
describeVpnConnectionsTest = do
    describe "describeVpnConnections doesn't fail" $ do
        it "describeVpnConnections doesn't throw any exception" $ do
            testEC2 region (describeVpnConnections [] []) `miss` anyConnectionException

describeCustomerGatewayTest :: Spec
describeCustomerGatewayTest = do
    describe "describeCustomerGateway doesn't fail" $ do
        it "describeCustomerGateway doesn't throw any exception" $ do
            testEC2 region (describeCustomerGateway [] []) `miss` anyConnectionException

describeInternetGatewaysTest :: Spec
describeInternetGatewaysTest = do
    describe "describeInternetGateways doesn't fail" $ do
        it "describeInternetGateways doesn't throw any exception" $ do
            testEC2 region (describeInternetGateways [] []) `miss` anyConnectionException

createVpcTest :: Spec
createVpcTest = do
    describe "createVpc doesn't fail" $ do
        it "createVpc and deleteVpc doesn't fail" $ do
            vpc <- testEC2' region (createVpc "80.0.0.0/16" Nothing)
            testEC2' region (deleteVpc $ vpcId vpc) `shouldReturn` True

describeDhcpOptionsTest :: Spec
describeDhcpOptionsTest = do
    describe "describeDhcpOptions doesn't fail" $ do
        it "describeDhcpOptions doesn't throw any exception" $ do
            testEC2 region (describeDhcpOptions [] []) `miss` anyConnectionException

createDhcpOptionsTest :: Spec
createDhcpOptionsTest = do
    describe "createDhcpOptions doesn't fail" $ do
        it "createDhcpOptions and deleteDhcpOptions doesn't fail" $ do
            options <- testEC2' region (createDhcpOptions [param])
            testEC2' region (deleteDhcpOptions $ dhcpOptionsId options) `shouldReturn` True
  where
    param = DhcpConfiguration "domain-name" [DhcpValue "example.com"]

vpnConnectionTest :: Spec
vpnConnectionTest = do
    describe "{create,delete}CustomerGateway, {create,delete}VpnGateway and {create,delete}VpnConnection don't fail" $ do
        it "{create,delete}CustomerGateway, {create,delete}VpnGateway and {create,delete}VpnConnection don't throw any exception" $ do
            testEC2' region (withVpn Nothing $ const (return ())) `miss` anyConnectionException

attachAndDetachVpnGatewayTest :: Spec
attachAndDetachVpnGatewayTest = do
    describe "{attach,detach}VpnGateway" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (do
                withVpnGateway CreateVpnGatewayTypeIpsec1 Nothing $ \VpnGateway{vpnGatewayId = vgw} -> do
                    withVpc "10.0.0.0/24" $ \Vpc{vpcId = vpc} -> do
                        withVpnGatewayAttached vgw vpc $ return ()
                        waitForVpnGatewayAttachment vgw vpc AttachmentStateDetached
                ) `miss` anyConnectionException

waitForVpnGatewayAttachment :: (MonadBaseControl IO m, MonadResource m) => Text -> Text -> AttachmentState -> EC2 m VpnGateway
waitForVpnGatewayAttachment vgw vpc s = wait p (\vgw' -> list $ describeVpnGateways [vgw'] []) vgw
  where
    p VpnGateway{vpnGatewayAttachments = as}
        | Just a <- find ((== vpc) . attachmentVpcId) as = attachmentState a == s
        | otherwise = True

internetGatewayTest :: Spec
internetGatewayTest = do
    describe "{create,delete,attach,detach}InternetGateway" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (
                withVpc "10.0.0.0/24" $ \Vpc{vpcId = vpc} ->
                    withInternetGateway $ \InternetGateway{internetGatewayInternetGatewayId = igw} ->
                        withInternetGatewayAttached igw vpc $ return ()
                ) `miss` anyConnectionException

associateDhcpOptionsTest :: Spec
associateDhcpOptionsTest = do
    describe "associateDhcpOptions" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (
                withDhcpOptions opts $ \DhcpOptions{dhcpOptionsId = dhcpOpt} ->
                    withVpc "10.0.0.0/24" $ \Vpc{vpcId = vpc} ->
                        associateDhcpOptions dhcpOpt vpc
                ) `miss` anyConnectionException
  where
    opts =
        [ DhcpConfiguration "domain-name" [DhcpValue "example.com"]
        , DhcpConfiguration "domain-name-servers" [DhcpValue "10.2.5.1", DhcpValue "10.2.5.2"]
        ]

createAndDeleteVpnConnectionRouteTest :: Spec
createAndDeleteVpnConnectionRouteTest = do
    describe "{create,delete}VpnConnectionRoute" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (
                withVpn (Just True) $ \cid -> do
                    wait
                        (\conn -> vpnConnectionState conn == VpnConnectionStateAvailable)
                        (\c -> list $ describeVpnConnections [c] [])
                        cid
                    let cidr = "11.12.0.0/16"
                    createVpnConnectionRoute cidr cid
                    deleteVpnConnectionRoute cidr cid
                ) `miss` anyConnectionException

withVpn :: (MonadBaseControl IO m, MonadResource m) => Maybe Bool -> (Text -> EC2 m a) -> EC2 m a
withVpn static f =
    withCustomerGateway "ipsec.1" "202.202.202.20" 65000 $ \CustomerGateway{customerGatewayId = cgid} -> do
        withVpnGateway CreateVpnGatewayTypeIpsec1 Nothing $ \VpnGateway{vpnGatewayId = vpnid} -> do
            (c,ret) <- withVpnConnection "ipsec.1" cgid vpnid Nothing static $ \VpnConnection{vpnConnectionId = cid} -> do
                waitVpnConnection VpnConnectionStateAvailable cid
                (cid,) <$> f cid
            waitVpnConnection VpnConnectionStateDeleted c
            return ret
  where
    waitVpnConnection state = wait
        (\connection -> vpnConnectionState connection == state)
        (\cid -> list $ describeVpnConnections [cid] [])

enableAndDisableVgwRoutePropagationTest :: Spec
enableAndDisableVgwRoutePropagationTest = do
    describe "{enable,disable}VgwRoutePropagation" $ do
        it "doesn't throw any exception" $ do
            testEC2' region (
                withVpnGateway CreateVpnGatewayTypeIpsec1 Nothing $ \VpnGateway{vpnGatewayId = vgw} ->
                    withVpc "10.0.0.0/24" $ \Vpc{vpcId = vpc} -> do
                        withVpnGatewayAttached vgw vpc $ do
                            withRouteTable vpc $ \RouteTable{routeTableId = rtb} -> do
                                waitForVpnGatewayAttachment vgw vpc AttachmentStateAttached
                                enableVgwRoutePropagation rtb vgw
                                disableVgwRoutePropagation rtb vgw
                        waitForVpnGatewayAttachment vgw vpc AttachmentStateDetached
                ) `miss` anyConnectionException
