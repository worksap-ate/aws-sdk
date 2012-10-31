{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.EC2.RouteTable
    ( associateRouteTable
    , createRouteTable
    , deleteRouteTable
    , describeRouteTables
    , disassociateRouteTable
    , replaceRouteTableAssociation
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

------------------------------------------------------------
-- describeRouteTables
------------------------------------------------------------
describeRouteTables
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ RouteTableIds
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m RouteTable)
describeRouteTables routeTables filters = do
    ec2QuerySource "DescribeRouteTables" params $
        itemConduit "routeTableSet" routeTableSink
  where
    params =
        [ ArrayParams "RouteTableId" routeTables
        , FilterParams filters
        ]

routeTableSink :: MonadThrow m
    => GLSink Event m RouteTable
routeTableSink = RouteTable
    <$> getT "routeTableId"
    <*> getT "vpcId"
    <*> routeSink
    <*> routeTableAssociationSink
    <*> getMT "propagatingVgwSet"
    <*> resourceTagSink

routeSink :: MonadThrow m
    => GLSink Event m [Route]
routeSink = itemsSet "routeSet" $ Route
    <$> getT "destinationCidrBlock"
    <*> getMT "gatewayId"
    <*> getMT "instanceId"
    <*> getMT "instanceOwnerId"
    <*> getMT "networkInterfaceId"
    <*> getF "state" routeState
    <*> getM "origin" (routeOrigin <$>)

routeTableAssociationSink :: MonadThrow m
    => GLSink Event m [RouteTableAssociation]
routeTableAssociationSink = itemsSet "associationSet" $ RouteTableAssociation
    <$> getT "routeTableAssociationId"
    <*> getT "routeTableId"
    <*> getMT "subnetId"
    <*> getM "main" (textToBool <$>)

------------------------------------------------------------
-- createRouteTable
------------------------------------------------------------
createRouteTable
    :: (MonadResource m, MonadBaseControl IO m)
    => Text
    -> EC2 m RouteTable
createRouteTable vid =
    ec2Query "CreateRouteTable" [ValueParam "VpcId" vid] $
        element "routeTable" routeTableSink

------------------------------------------------------------
-- deleteRouteTable
------------------------------------------------------------
deleteRouteTable
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ RouteTableId
    -> EC2 m Bool
deleteRouteTable rtid =
    ec2Query "DeleteRouteTable" [ValueParam "RouteTableId" rtid]
        $ getF "return" textToBool

------------------------------------------------------------
-- associateRouteTable
------------------------------------------------------------
associateRouteTable
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ RouteTableId
    -> Text -- ^ SubnetId
    -> EC2 m Text -- ^ associationId
associateRouteTable rtid sid =
    ec2Query "AssociateRouteTable" params
        $ getT "associationId"
  where
    params = [ ValueParam "RouteTableId" rtid
             , ValueParam "SubnetId" sid
             ]

------------------------------------------------------------
-- disassociateRouteTable
------------------------------------------------------------
disassociateRouteTable
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ AssociationId
    -> EC2 m Bool -- ^ return
disassociateRouteTable aid =
   ec2Query "DisassociateRouteTable" [ValueParam "AssociationId" aid]
        $ getF "return" textToBool

------------------------------------------------------------
-- replaceRouteTableAssociation
------------------------------------------------------------
replaceRouteTableAssociation
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ AssociationId
    -> Text -- ^ RouteTableId
    -> EC2 m Text -- ^ newAssociationId
replaceRouteTableAssociation aid rtid =
    ec2Query "ReplaceRouteTableAssociation" params
        $ getT "newAssociationId"
  where
    params = [ ValueParam "AssociationId" aid
             , ValueParam "RouteTableId" rtid
             ]
