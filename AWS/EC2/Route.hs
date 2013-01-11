{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module AWS.EC2.Route
    ( createRoute
    , deleteRoute
    , replaceRoute
    ) where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.ByteString (ByteString)
import Data.Conduit (MonadResource)
import Data.IP (AddrRange, IPv4)
import Data.Text (Text)

import AWS.EC2.Internal (EC2)
import AWS.EC2.Query (ec2Query, (|=))
import AWS.EC2.Types (CreateRouteRequest(..))
import AWS.Lib.Parser (getT)
import AWS.Util (toText)

createRoute
    :: (MonadResource m, MonadBaseControl IO m)
    => CreateRouteRequest
    -> EC2 m Bool
createRoute = routeRequest "CreateRoute"

deleteRoute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ The ID of the route table.
    -> AddrRange IPv4 -- ^ The CIDR range of the destination for the route to delete.
    -> EC2 m Bool
deleteRoute tableId cidrBlock =
    ec2Query "DeleteRoute"
        [ "RouteTableId" |= tableId
        , "DestinationCidrBlock" |= toText cidrBlock
        ] $ getT "return"

replaceRoute
    :: (MonadResource m, MonadBaseControl IO m)
    => CreateRouteRequest
    -> EC2 m Bool
replaceRoute = routeRequest "ReplaceRoute"

routeRequest
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> CreateRouteRequest
    -> EC2 m Bool
routeRequest action (CreateRouteToGateway{..}) =
      routeRequest' action createRouteTableId createRouteDestinationCidrBlock "GatewayId" createRouteGatewayId
routeRequest action (CreateRouteToInstance{..}) =
      routeRequest' action createRouteTableId createRouteDestinationCidrBlock "InstanceId" createRouteInstanceId
routeRequest action (CreateRouteToNetworkInterface{..}) =
      routeRequest' action createRouteTableId createRouteDestinationCidrBlock "NetworkInterfaceId" createRouteNetworkInterfaceId

routeRequest'
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> Text
    -> AddrRange IPv4
    -> Text
    -> Text
    -> EC2 m Bool
routeRequest' action tableId cidrBlock targetName targetId =
    ec2Query action
        [ "RouteTableId" |= tableId
        , "DestinationCidrBlock" |= toText cidrBlock
        , targetName |= targetId
        ] $ getT "return"

