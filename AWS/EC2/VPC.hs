{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.EC2.VPC
    ( associateDhcpOptions
    , attachInternetGateway
    , createVpc
    , createVpnConnection
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
        [ "InternetGatewayId" |= internetGatewayId
        , "VpcId" |= vid ]

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
        [ "InternetGatewayId" |= internetGatewayId
        , "VpcId" |= vid ]

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
        [ "InternetGatewayId" |.#= internetGatewayIds
        , filtersParam filters
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

------------------------------------------------------------
-- describeVpnConnections
------------------------------------------------------------
describeVpnConnections
    :: (MonadBaseControl IO m, MonadResource m)
    => [Text] -- ^ VpnConnectionIds
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m VpnConnection)
describeVpnConnections ids filters =
    ec2QuerySource "DescribeVpnConnections" params $
        itemConduit "vpnConnectionSet" vpnConnectionSink
  where
    params =
        [ "VpnConnectionId" |.#= ids
        , filtersParam filters
        ]

vpnConnectionSink
    :: MonadThrow m
    => GLSink Event m VpnConnection
vpnConnectionSink = VpnConnection
    <$> getT "vpnConnectionId"
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
-- createVpnConnection
------------------------------------------------------------
createVpnConnection
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ Type. The valid value is ipsec.1
    -> Text -- ^ CustomerGatewayId
    -> Text -- ^ VpnGatewayId
    -> Maybe Text -- ^ AvailabilityZone
    -> Maybe Bool -- ^ Option
    -> EC2 m VpnConnection
createVpnConnection type' cgid vgid zone option =
    ec2Query "CreateVpnConnection" params $
        element "vpnConnection" vpnConnectionSink
  where
    params =
        [ "Type" |= type'
        , "CustomerGatewayId" |= cgid
        , "VpnGatewayId" |= vgid
        , "AvailabilityZone" |=? zone
        , "Options" |.+ "StaticRoutesOnly" |=? boolToText <$> option
        ]

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
        [ "VpcId" |.#= vpcIds
        , filtersParam filters
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
        [ "CidrBlock" |= toText cidrBlock
        , "instanceTenancy" |=? instanceTenancy
        ]

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
        [ "VpnGatewayId" |.#= ids
        , filtersParam filters
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
createVpnGateway type' availabilityZone = do
    ec2Query "CreateVpnGateway" params $
        element "vpnGateway" vpnGatewaySink
  where
    params =
        [ "Type" |= createVpnGatewayTypeText type'
        , "AvailabilityZone" |=? availabilityZone
        ]
    createVpnGatewayTypeText CreateVpnGatewayTypeIpsec1 = "ipsec.1"

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
        [ "CustomerGatewayId" |.#= ids
        , filtersParam filters
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
        [ "Type" |= type'
        , "IpAddress" |= toText ipAddr
        , "BgpAsn" |= toText bgpAsn
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
        [ "DhcpOptionsId" |.#= ids
        , filtersParam filters
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
    params = ["DhcpConfiguration" |.#. map dhcpConfigurationParams confs]
    dhcpConfigurationParams conf =
        [ "Key" |= dhcpConfigurationKey conf
        , "Value" |.#= map dhcpValueValue (dhcpConfigurationDhcpValueSet conf)
        ]

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
    params = ["DhcpOptionsId" |= doid]

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
        [ "DhcpOptionsId" |= doid
        , "VpcId" |= vpcid
        ]
