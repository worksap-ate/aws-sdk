{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.EC2.VPC
    ( createVpc
    , createVpnGateway
    , createInternetGateway
    , deleteVpc
    , deleteVpnGateway
    , deleteInternetGateway
    , describeVpnConnections
    , describeVpnGateways
    , describeVpcs
    , describeInternetGateways
    ) where

import Data.Text (Text)

import Data.XML.Types (Event)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative

import AWS.EC2.Convert
import AWS.EC2.Internal
import AWS.EC2.Types
import AWS.EC2.Query
import AWS.Lib.Parser
import AWS.Util

import Debug.Trace

------------------------------------------------------------
-- deleteInternetGateway
------------------------------------------------------------
deleteInternetGateway
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ InternetGatewayId
    -> EC2 m Bool
deleteInternetGateway internetGatewayId =
    ec2Query "DeleteInternetGateway" [ ValueParam "InternetGatewayId" internetGatewayId ] $
        getF "return" textToBool

------------------------------------------------------------
-- createInternetGateway
------------------------------------------------------------
createInternetGateway
    :: (MonadResource m, MonadBaseControl IO m)
    => EC2 m InternetGateway
createInternetGateway =
    ec2Query "CreateInternetGateway" params $
        element "internetGateway" internetGatewaySink
  where
    params = []

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
    <*> getF "state" internetGatewayAttachmentState'
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
    <*> getF "state" vpnConnectionState
    <*> getT "customerGatewayConfiguration"
    <*> getT "type"
    <*> getT "customerGatewayId"
    <*> getT "vpnGatewayId"
    <*> resourceTagSink
    <*> itemsSet "vgwTelemetry"
        (VpnTunnelTelemetry
        <$> getT "outsideIpAddress"
        <*> getF "status" vpnTunnelTelemetryStatus
        <*> getF "lastStatusChange" textToTime
        <*> getT "statusMessage"
        <*> getF "acceptedRouteCount" textToInt
        )
    <*> elementM "options"
        (VpnConnectionOptionsRequest
        <$> getF "staticRoutesOnly" textToBool
        )
    <*> elementM "routes"
        (VpnStaticRoute
        <$> getT "destinationCidrBlock"
        <*> getF "source" vpnStaticRouteSource
        <*> getF "state" vpnStaticRouteState
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
    <*> getF "state" vpcState'
    <*> getT "cidrBlock"
    <*> getT "dhcpOptionsId"
    <*> resourceTagSink
    <*> getT "instanceTenancy"

------------------------------------------------------------
-- createVpc
------------------------------------------------------------
createVpc
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ CidrBlock
    -> Maybe Text -- ^ instanceTenancy
    -> EC2 m Vpc
createVpc cidrBlock instanceTenancy =
    ec2Query "CreateVpc" params $
        element "vpc" vpcSink
  where
    params =
        [ ValueParam "CidrBlock" cidrBlock
        ] ++ maybeParams [ ("instanceTenancy", instanceTenancy) ]

------------------------------------------------------------
-- deleteVpc
------------------------------------------------------------
deleteVpc
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ VpcId
    -> EC2 m Bool
deleteVpc vid =
    ec2Query "DeleteVpc" [ ValueParam "VpcId" vid ] $
        getF "return" textToBool

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
    <*> getF "state" vpnGatewayState'
    <*> getT "type"
    <*> getMT "availabilityZone"
    <*> itemsSet "attachments" attachmentSink
    <*> resourceTagSink

attachmentSink :: MonadThrow m
    => GLSink Event m Attachment
attachmentSink = Attachment
    <$> getT "vpcId"
    <*> getF "state" attachmentState'

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
deleteVpnGateway vgId = do
    ec2Query "DeleteVpnGateway" [ ValueParam "VpnGatewayId" vgId ] $
        getF "return" textToBool
