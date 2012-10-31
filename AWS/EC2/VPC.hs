{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.EC2.VPC
    ( describeVpnConnections
    , describeVpcs
    ) where

import Data.Text (Text)

import Data.XML.Types (Event)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative

import AWS.EC2.Internal
import AWS.EC2.Types
import AWS.EC2.Query
import AWS.Lib.Parser
import AWS.Util

import Debug.Trace

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
