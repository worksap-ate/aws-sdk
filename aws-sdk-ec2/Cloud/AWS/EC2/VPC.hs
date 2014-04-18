{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Cloud.AWS.EC2.VPC
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
import Data.Conduit
import Data.IP (IPv4, AddrRange)
import Control.Applicative
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)

import Cloud.AWS.Lib.Parser.Unordered (XmlElement, elementM, element, (.<))

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Query

------------------------------------------------------------
-- attachInternetGateway
------------------------------------------------------------
attachInternetGateway
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ InternetGatewayId
    -> Text -- ^ VpcId
    -> EC2 m Bool
attachInternetGateway internetGatewayId vid =
    ec2Query "AttachInternetGateway" params (.< "return")
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
    ec2Query "AttachVpnGateway" params $
        element "attachment" attachmentConv
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
    ec2Query "DetachInternetGateway" params (.< "return")
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
    ec2Query "DetachVpnGateway" params (.< "return")
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
        element "internetGateway" internetGatewayConv

------------------------------------------------------------
-- describeInternetGateways
------------------------------------------------------------
describeInternetGateways
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ InternetGatewayIds
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m InternetGateway)
describeInternetGateways internetGatewayIds filters = do
    ec2QuerySource "DescribeInternetGateways" params path $
        itemConduit internetGatewayConv
  where
    path = itemsPath "internetGatewaySet"
    params =
        [ "InternetGatewayId" |.#= internetGatewayIds
        , filtersParam filters
        ]

internetGatewayConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m InternetGateway
internetGatewayConv xml = InternetGateway
    <$> xml .< "internetGatewayId"
    <*> itemsSet "attachmentSet" internetGatewayAttachmentConv xml
    <*> resourceTagConv xml

internetGatewayAttachmentConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m InternetGatewayAttachment
internetGatewayAttachmentConv xml = InternetGatewayAttachment
    <$> xml .< "vpcId"
    <*> xml .< "state"

------------------------------------------------------------
-- describeVpnConnections
------------------------------------------------------------
describeVpnConnections
    :: (MonadBaseControl IO m, MonadResource m)
    => [Text] -- ^ VpnConnectionIds
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m VpnConnection)
describeVpnConnections ids filters =
    ec2QuerySource "DescribeVpnConnections" params path $
        itemConduit vpnConnectionConv
  where
    path = itemsPath "vpnConnectionSet"
    params =
        [ "VpnConnectionId" |.#= ids
        , filtersParam filters
        ]

vpnConnectionConv
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m VpnConnection
vpnConnectionConv xml = VpnConnection
    <$> xml .< "vpnConnectionId"
    <*> xml .< "state"
    <*> xml .< "customerGatewayConfiguration"
    <*> xml .< "type"
    <*> xml .< "customerGatewayId"
    <*> xml .< "vpnGatewayId"
    <*> resourceTagConv xml
    <*> itemsSet "vgwTelemetry"
        (\xml' -> VpnTunnelTelemetry
        <$> xml' .< "outsideIpAddress"
        <*> xml' .< "status"
        <*> xml' .< "lastStatusChange"
        <*> xml' .< "statusMessage"
        <*> xml' .< "acceptedRouteCount"
        ) xml
    <*> elementM "options"
        (\xml' -> VpnConnectionOptionsRequest
        <$> xml' .< "staticRoutesOnly"
        ) xml
    <*> itemsSet "routes"
        (\xml' -> VpnStaticRoute
        <$> xml' .< "destinationCidrBlock"
        <*> xml' .< "source"
        <*> xml' .< "state"
        ) xml

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
        element "vpnConnection" vpnConnectionConv
  where
    params =
        [ "Type" |= type'
        , "CustomerGatewayId" |= cgid
        , "VpnGatewayId" |= vgid
        , "AvailabilityZone" |=? zone
        , "Options" |.+ "StaticRoutesOnly" |=? option
        ]

createVpnConnectionRoute
    :: (MonadBaseControl IO m, MonadResource m)
    => AddrRange IPv4 -- ^ The CIDR block associated with the local subnet of the customer data center.
    -> Text -- ^ The ID of the VPN connection.
    -> EC2 m Bool
createVpnConnectionRoute cidr vpnConn =
    ec2Query "CreateVpnConnectionRoute" params (.< "return")
  where
    params =
        [ "DestinationCidrBlock" |= cidr
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
    ec2Query "DeleteVpnConnectionRoute" params (.< "return")
  where
    params =
        [ "DestinationCidrBlock" |= cidr
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
    ec2QuerySource "DescribeVpcs" params path $
        itemConduit vpcConv
  where
    path = itemsPath "vpcSet"
    params =
        [ "VpcId" |.#= vpcIds
        , filtersParam filters
        ]

vpcConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m Vpc
vpcConv xml = Vpc
    <$> xml .< "vpcId"
    <*> xml .< "state"
    <*> xml .< "cidrBlock"
    <*> xml .< "dhcpOptionsId"
    <*> resourceTagConv xml
    <*> xml .< "instanceTenancy"
    <*> xml .< "isDefault"

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
        element "vpc" vpcConv
  where
    params =
        [ "CidrBlock" |= cidrBlock
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
    ec2QuerySource "DescribeVpnGateways" params path $
        itemConduit vpnGatewayConv
  where
    path = itemsPath "vpnGatewaySet"
    params =
        [ "VpnGatewayId" |.#= ids
        , filtersParam filters
        ]

vpnGatewayConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m VpnGateway
vpnGatewayConv xml = VpnGateway
    <$> xml .< "vpnGatewayId"
    <*> xml .< "state"
    <*> xml .< "type"
    <*> xml .< "availabilityZone"
    <*> itemsSet "attachments" attachmentConv xml
    <*> resourceTagConv xml

attachmentConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m Attachment
attachmentConv xml = Attachment
    <$> xml .< "vpcId"
    <*> xml .< "state"

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
        element "vpnGateway" vpnGatewayConv
  where
    params =
        [ "Type" |= type'
        , "AvailabilityZone" |=? availabilityZone
        ]

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
    ec2QuerySource "DescribeCustomerGateways" params path $
        itemConduit customerGatewayConv
  where
    path = itemsPath "customerGatewaySet"
    params =
        [ "CustomerGatewayId" |.#= ids
        , filtersParam filters
        ]

customerGatewayConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m CustomerGateway
customerGatewayConv xml = CustomerGateway
    <$> xml .< "customerGatewayId"
    <*> xml .< "state"
    <*> xml .< "type"
    <*> xml .< "ipAddress"
    <*> xml .< "bgpAsn"
    <*> resourceTagConv xml

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
        element "customerGateway" customerGatewayConv
  where
    params =
        [ "Type" |= type'
        , "IpAddress" |= ipAddr
        , "BgpAsn" |= bgpAsn
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
    ec2QuerySource "DescribeDhcpOptions" params path $
        itemConduit dhcpOptionsConv
  where
    path = itemsPath "dhcpOptionsSet"
    params =
        [ "DhcpOptionsId" |.#= ids
        , filtersParam filters
        ]

dhcpOptionsConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m DhcpOptions
dhcpOptionsConv xml = DhcpOptions
    <$> xml .< "dhcpOptionsId"
    <*> itemsSet "dhcpConfigurationSet" dhcpConfigurationConv xml
    <*> resourceTagConv xml

dhcpConfigurationConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m DhcpConfiguration
dhcpConfigurationConv xml = DhcpConfiguration
    <$> xml .< "key"
    <*> itemsSet "valueSet"
        (\xml' -> DhcpValue
        <$> xml' .< "value"
        ) xml

------------------------------------------------------------
-- createDhcpOptions
------------------------------------------------------------
createDhcpOptions
    :: (MonadResource m, MonadBaseControl IO m)
    => [DhcpConfiguration] -- ^ DhcpConfigurations
    -> EC2 m DhcpOptions
createDhcpOptions confs =
    ec2Query "CreateDhcpOptions" params $
        element "dhcpOptions" dhcpOptionsConv
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
    ec2Query "DeleteDhcpOptions" params (.< "return")
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
    ec2Query "AssociateDhcpOptions" params (.< "return")
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
    ec2Query "EnableVgwRoutePropagation" params (.< "return")
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
    ec2Query "DisableVgwRoutePropagation" params (.< "return")
  where
    params =
        [ "RouteTableId" |= rtb
        , "GatewayId" |= vgw
        ]
