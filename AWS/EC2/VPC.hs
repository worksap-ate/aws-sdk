{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.EC2.VPC
    ( associateDhcpOptions
    , attachInternetGateway
    , createVpc
    , createVpnGateway
    , createCustomerGateway
    , createInternetGateway
    , createDhcpOptions
    , deleteVpc
    , deleteVpnGateway
    , deleteCustomerGateway
    , deleteInternetGateway
    , deleteDhcpOptions
    , describeVpnConnections
    , describeVpnGateways
    , describeVpcs
    , describeCustomerGateway
    , describeInternetGateways
    , describeDhcpOptions
    , detachInternetGateway
    ) where

import Data.Text (Text)
import Data.XML.Types (Event)
import Data.Conduit
import Data.IP (IPv4, AddrRange)
import Data.Monoid ((<>))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative

import AWS.EC2.Internal
import AWS.EC2.Types
import AWS.EC2.Query
import AWS.Lib.Parser
import AWS.Util

import Debug.Trace

------------------------------------------------------------
-- attachInternetGateway
------------------------------------------------------------
attachInternetGateway
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ InternetGatewayId
    -> Text -- ^ VpcId
    -> EC2 m Bool
attachInternetGateway internetGatewayId vid =
    ec2Query "AttachInternetGateway" params $ getT "return"
  where
    params =
        [ ValueParam "InternetGatewayId" internetGatewayId
        , ValueParam "VpcId" vid ]

------------------------------------------------------------
-- detachInternetGateway
------------------------------------------------------------
detachInternetGateway
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ InternetGatewayId
    -> Text -- ^ VpcId
    -> EC2 m Bool
detachInternetGateway internetGatewayId vid =
    ec2Query "DetachInternetGateway" params $ getT "return"
  where
    params =
        [ ValueParam "InternetGatewayId" internetGatewayId
        , ValueParam "VpcId" vid ]

------------------------------------------------------------
-- deleteInternetGateway
------------------------------------------------------------
deleteInternetGateway
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ InternetGatewayId
    -> EC2 m Bool
deleteInternetGateway = ec2Delete "DeleteInternetGateway" "InternetGatewayId"

------------------------------------------------------------
-- createInternetGateway
------------------------------------------------------------
createInternetGateway
    :: (MonadResource m, MonadBaseControl IO m)
    => EC2 m InternetGateway
createInternetGateway =
    ec2Query "CreateInternetGateway" [] $
        element "internetGateway" internetGatewaySink

------------------------------------------------------------
-- describeInternetGateways
------------------------------------------------------------
describeInternetGateways
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ InternetGatewayIds
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m InternetGateway)
describeInternetGateways internetGatewayIds filters = do
    ec2QuerySource "DescribeInternetGateways" params $
        itemConduit "internetGatewaySet" internetGatewaySink
  where
    params =
        [ ArrayParams "InternetGatewayId" internetGatewayIds
        , FilterParams filters
        ]

internetGatewaySink :: MonadThrow m
    => GLSink Event m InternetGateway
internetGatewaySink = InternetGateway
    <$> getT "internetGatewayId"
    <*> itemsSet "attachmentSet" internetGatewayAttachmentSink
    <*> resourceTagSink

internetGatewayAttachmentSink :: MonadThrow m
    => GLSink Event m InternetGatewayAttachment
internetGatewayAttachmentSink = InternetGatewayAttachment
    <$> getT "vpcId"
    <*> getT "state"
describeVpnConnections
    :: (MonadBaseControl IO m, MonadResource m)
    => [Text] -- ^ VpnConnectionIds
    -> [Filter]
    -> EC2 m (ResumableSource m VpnConnection)
describeVpnConnections ids filters =
    ec2QuerySource "DescribeVpnConnections" params vpnConnectionConduit
  where
    params = [ArrayParams "VpnConnectionId" ids]
        ++ [FilterParams filters]

vpnConnectionConduit
    :: (MonadBaseControl IO m, MonadResource m)
    => GLConduit Event m VpnConnection
vpnConnectionConduit = itemConduit "vpnConnectionSet" $
    VpnConnection
    <$> do
        a <- getT "vpnConnectionId"
        traceShow a $ return a
    <*> getT "state"
    <*> getT "customerGatewayConfiguration"
    <*> getT "type"
    <*> getT "customerGatewayId"
    <*> getT "vpnGatewayId"
    <*> resourceTagSink
    <*> itemsSet "vgwTelemetry"
        (VpnTunnelTelemetry
        <$> getT "outsideIpAddress"
        <*> getT "status"
        <*> getT "lastStatusChange"
        <*> getT "statusMessage"
        <*> getT "acceptedRouteCount"
        )
    <*> elementM "options"
        (VpnConnectionOptionsRequest
        <$> getT "staticRoutesOnly"
        )
    <*> elementM "routes"
        (VpnStaticRoute
        <$> getT "destinationCidrBlock"
        <*> getT "source"
        <*> getT "state"
        )

------------------------------------------------------------
-- describeVpcs
------------------------------------------------------------
describeVpcs
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ VpcIds
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m Vpc)
describeVpcs vpcIds filters = do
    ec2QuerySource "DescribeVpcs" params $
        itemConduit "vpcSet" vpcSink
  where
    params =
        [ ArrayParams "VpcId" vpcIds
        , FilterParams filters
        ]

vpcSink :: MonadThrow m
    => GLSink Event m Vpc
vpcSink = Vpc
    <$> getT "vpcId"
    <*> getT "state"
    <*> getT "cidrBlock"
    <*> getT "dhcpOptionsId"
    <*> resourceTagSink
    <*> getT "instanceTenancy"
    <*> getT "isDefault"

------------------------------------------------------------
-- createVpc
------------------------------------------------------------
createVpc
    :: (MonadResource m, MonadBaseControl IO m)
    => AddrRange IPv4 -- ^ CidrBlock
    -> Maybe Text -- ^ instanceTenancy
    -> EC2 m Vpc
createVpc cidrBlock instanceTenancy =
    ec2Query "CreateVpc" params $
        element "vpc" vpcSink
  where
    params =
        [ ValueParam "CidrBlock" $ toText cidrBlock
        ] ++ maybeParams [ ("instanceTenancy", instanceTenancy) ]

------------------------------------------------------------
-- deleteVpc
------------------------------------------------------------
deleteVpc
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VpcId
    -> EC2 m Bool
deleteVpc = ec2Delete "DeleteVpc" "VpcId"

------------------------------------------------------------
-- describeVpnGateways
------------------------------------------------------------
describeVpnGateways
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ VpnGatewayId
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m VpnGateway)
describeVpnGateways ids filters = do
    ec2QuerySource "DescribeVpnGateways" params $
        itemConduit "vpnGatewaySet" vpnGatewaySink
  where
    params =
        [ ArrayParams "VpnGatewayId" ids
        , FilterParams filters
        ]

vpnGatewaySink :: MonadThrow m
    => GLSink Event m VpnGateway
vpnGatewaySink = VpnGateway
    <$> getT "vpnGatewayId"
    <*> getT "state"
    <*> getT "type"
    <*> getT "availabilityZone"
    <*> itemsSet "attachments" attachmentSink
    <*> resourceTagSink

attachmentSink :: MonadThrow m
    => GLSink Event m Attachment
attachmentSink = Attachment
    <$> getT "vpcId"
    <*> getT "state"

------------------------------------------------------------
-- createVpnGateway
------------------------------------------------------------
createVpnGateway
    :: (MonadResource m, MonadBaseControl IO m)
    => CreateVpnGatewayType -- ^ Type. The valid value is CreateVpnGatewayTypeIpsec1
    -> Maybe Text -- ^ AvailabilityZone
    -> EC2 m VpnGateway
createVpnGateway _ availabilityZone = do
    ec2Query "CreateVpnGateway" params $
        element "vpnGateway" vpnGatewaySink
  where
    params =
        [ ValueParam "Type" "ipsec.1"
        ] ++ maybeParams [ ("AvailabilityZone", availabilityZone) ]

------------------------------------------------------------
-- deleteVpnGateway
------------------------------------------------------------
deleteVpnGateway
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VpnGatewayId
    -> EC2 m Bool
deleteVpnGateway = ec2Delete "DeleteVpnGateway" "VpnGatewayId"

------------------------------------------------------------
-- describeCustomerGateway
------------------------------------------------------------
describeCustomerGateway
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ CustomerGatewayId
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m CustomerGateway)
describeCustomerGateway ids filters = do
    ec2QuerySource "DescribeCustomerGateways" params $
        itemConduit "customerGatewaySet" customerGatewaySink
  where
    params =
        [ ArrayParams "CustomerGatewayId" ids
        , FilterParams filters
        ]

customerGatewaySink :: MonadThrow m
    => GLSink Event m CustomerGateway
customerGatewaySink = CustomerGateway
    <$> getT "customerGatewayId"
    <*> getT "state"
    <*> getT "type"
    <*> getT "ipAddress"
    <*> getT "bgpAsn"
    <*> resourceTagSink

------------------------------------------------------------
-- createCustomerGateway
------------------------------------------------------------
createCustomerGateway
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ Type
    -> IPv4 -- ^ IpAddress
    -> Int -- ^ BgpAsn
    -> EC2 m CustomerGateway
createCustomerGateway type' ipAddr bgpAsn = do
    ec2Query "CreateCustomerGateway" params $
        element "customerGateway" customerGatewaySink
  where
    params =
        [ ValueParam "Type" type' 
        , ValueParam "IpAddress" $ toText ipAddr
        , ValueParam "BgpAsn" (toText bgpAsn)
        ]

------------------------------------------------------------
-- deleteCustomerGateway
------------------------------------------------------------
deleteCustomerGateway
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ CustomerGatewayId
    -> EC2 m Bool
deleteCustomerGateway = ec2Delete "DeleteCustomerGateway" "CustomerGatewayId"

------------------------------------------------------------
-- describeDhcpOptions
------------------------------------------------------------
describeDhcpOptions
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ DhcpOptionsIds
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m DhcpOptions)
describeDhcpOptions ids filters =
    ec2QuerySource "DescribeDhcpOptions" params $
        itemConduit "dhcpOptionsSet" dhcpOptionsSink
  where
    params =
        [ ArrayParams "DhcpOptionsId" ids
        , FilterParams filters
        ]

dhcpOptionsSink :: MonadThrow m
    => GLSink Event m DhcpOptions
dhcpOptionsSink = DhcpOptions
    <$> getT "dhcpOptionsId"
    <*> itemsSet "dhcpConfigurationSet" dhcpConfigurationSink
    <*> resourceTagSink

dhcpConfigurationSink :: MonadThrow m
    => GLSink Event m DhcpConfiguration
dhcpConfigurationSink = DhcpConfiguration
    <$> getT "key"
    <*> itemsSet "valueSet"
        (DhcpValue
        <$> getT "value"
        )

------------------------------------------------------------
-- createDhcpOptions
------------------------------------------------------------
createDhcpOptions
    :: (MonadResource m, MonadBaseControl IO m)
    => [DhcpConfiguration] -- ^ DhcpConfigurations
    -> EC2 m DhcpOptions
createDhcpOptions confs =
    ec2Query "CreateDhcpOptions" params $
        element "dhcpOptions" dhcpOptionsSink
  where
    numTexts = map (("DhcpConfiguration." <>) . toText) ([1..] :: [Int])
    f (t, conf) =
        [ ValueParam (t <> ".Key") $
            dhcpConfigurationKey conf
        , ArrayParams (t <> ".Value") $
            map dhcpValueValue $
            dhcpConfigurationDhcpValueSet conf
        ]
    params = concat $ map f $ zip numTexts confs

------------------------------------------------------------
-- deleteDhcpOptions
------------------------------------------------------------
deleteDhcpOptions
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ DhcpOptionsId
    -> EC2 m Bool
deleteDhcpOptions doid =
    ec2Query "DeleteDhcpOptions" params $ getT "return"
  where
    params = [ValueParam "DhcpOptionsId" doid]

------------------------------------------------------------
-- associateDhcpOptions
------------------------------------------------------------
associateDhcpOptions
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ DhcpOptionsId
    -> Text -- ^ VpcId
    -> EC2 m Bool
associateDhcpOptions doid vpcid =
    ec2Query "AssociateDhcpOptions" params $ getT "return"
  where
    params =
        [ ValueParam "DhcpOptionsId" doid
        , ValueParam "VpcId" vpcid
        ]
