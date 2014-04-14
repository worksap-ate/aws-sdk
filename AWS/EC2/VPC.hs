{-# LANGUAGE FlexibleContexts, RankNTypes, CPP #-}

module AWS.EC2.VPC
    ( associateDhcpOptions
    , attachInternetGateway
    , attachVpnGateway
    , createVpc
    , createVpnConnection
    , createVpnConnectionRoute
    , createVpnGateway
    , createCustomerGateway
    , createInternetGateway
    , createDhcpOptions
    , deleteVpc
    , deleteVpnConnection
    , deleteVpnConnectionRoute
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
    , detachVpnGateway
    , disableVgwRoutePropagation
    , enableVgwRoutePropagation
    ) where

import Data.Text (Text)
import Data.XML.Types (Event)
import Data.Conduit
import Data.IP (IPv4, AddrRange)
import Control.Applicative
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadThrow, MonadBaseControl, MonadResource)
#endif

import AWS.EC2.Internal
import AWS.EC2.Types
import AWS.EC2.Query
import AWS.Lib.Parser
import AWS.Util

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

attachVpnGateway
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the virtual private gateway.
    -> Text -- ^ The ID of the VPC.
    -> EC2 m Attachment
attachVpnGateway vgw vpc =
    ec2Query "AttachVpnGateway" params $ element "attachment" attachmentSink
  where
    params =
        [ "VpnGatewayId" |= vgw
        , "VpcId" |= vpc
        ]

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

detachVpnGateway
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the virtual private gateway.
    -> Text -- ^ The ID of the VPC.
    -> EC2 m Bool
detachVpnGateway vgw vpc =
    ec2Query "DetachVpnGateway" params $ getT "return"
  where
    params =
        [ "VpnGatewayId" |= vgw
        , "VpcId" |= vpc
        ]

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
    => Consumer Event m InternetGateway
internetGatewaySink = InternetGateway
    <$> getT "internetGatewayId"
    <*> itemsSet "attachmentSet" internetGatewayAttachmentSink
    <*> resourceTagSink

internetGatewayAttachmentSink :: MonadThrow m
    => Consumer Event m InternetGatewayAttachment
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
    => Consumer Event m VpnConnection
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
    <*> itemsSet "routes"
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

createVpnConnectionRoute
    :: (MonadBaseControl IO m, MonadResource m)
    => AddrRange IPv4 -- ^ The CIDR block associated with the local subnet of the customer data center.
    -> Text -- ^ The ID of the VPN connection.
    -> EC2 m Bool
createVpnConnectionRoute cidr vpnConn =
    ec2Query "CreateVpnConnectionRoute" params $ getT "return"
  where
    params =
        [ "DestinationCidrBlock" |= toText cidr
        , "VpnConnectionId" |= vpnConn
        ]

------------------------------------------------------------
-- deleteVpnConnection
------------------------------------------------------------
deleteVpnConnection
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VpnConnectionId
    -> EC2 m Bool
deleteVpnConnection = ec2Delete "DeleteVpnConnection" "VpnConnectionId"

deleteVpnConnectionRoute
    :: (MonadBaseControl IO m, MonadResource m)
    => AddrRange IPv4 -- ^ The CIDR block associated with the local subnet of the customer data center.
    -> Text -- ^ The ID of the VPN connection.
    -> EC2 m Bool
deleteVpnConnectionRoute cidr vpnConn =
    ec2Query "DeleteVpnConnectionRoute" params $ getT "return"
  where
    params =
        [ "DestinationCidrBlock" |= toText cidr
        , "VpnConnectionId" |= vpnConn
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
    => Consumer Event m Vpc
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
    => Consumer Event m VpnGateway
vpnGatewaySink = VpnGateway
    <$> getT "vpnGatewayId"
    <*> getT "state"
    <*> getT "type"
    <*> getT "availabilityZone"
    <*> itemsSet "attachments" attachmentSink
    <*> resourceTagSink

attachmentSink :: MonadThrow m
    => Consumer Event m Attachment
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
    => Consumer Event m CustomerGateway
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
    => Consumer Event m DhcpOptions
dhcpOptionsSink = DhcpOptions
    <$> getT "dhcpOptionsId"
    <*> itemsSet "dhcpConfigurationSet" dhcpConfigurationSink
    <*> resourceTagSink

dhcpConfigurationSink :: MonadThrow m
    => Consumer Event m DhcpConfiguration
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

enableVgwRoutePropagation
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the routing table.
    -> Text -- ^ The ID of the virtual private gateway.
    -> EC2 m Bool
enableVgwRoutePropagation rtb vgw =
    ec2Query "EnableVgwRoutePropagation" params $ getT "return"
  where
    params =
        [ "RouteTableId" |= rtb
        , "GatewayId" |= vgw
        ]

disableVgwRoutePropagation
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The ID of the routing table.
    -> Text -- ^ The ID of the virtual private gateway.
    -> EC2 m Bool
disableVgwRoutePropagation rtb vgw =
    ec2Query "DisableVgwRoutePropagation" params $ getT "return"
  where
    params =
        [ "RouteTableId" |= rtb
        , "GatewayId" |= vgw
        ]
