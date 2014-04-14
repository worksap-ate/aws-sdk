{-# LANGUAGE FlexibleContexts, RankNTypes, CPP #-}

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
import Control.Applicative
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadThrow, MonadBaseControl, MonadResource)
#endif

import AWS.EC2.Internal
import AWS.EC2.Types
import AWS.EC2.Query
import AWS.Lib.Parser

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
        [ "RouteTableId" |.#= routeTables
        , filtersParam filters
        ]

routeTableSink :: MonadThrow m
    => Consumer Event m RouteTable
routeTableSink = RouteTable
    <$> getT "routeTableId"
    <*> getT "vpcId"
    <*> routeSink
    <*> routeTableAssociationSink
    <*> getT "propagatingVgwSet"
    <*> resourceTagSink

routeSink :: MonadThrow m
    => Consumer Event m [Route]
routeSink = itemsSet "routeSet" $ Route
    <$> getT "destinationCidrBlock"
    <*> getT "gatewayId"
    <*> getT "instanceId"
    <*> getT "instanceOwnerId"
    <*> getT "networkInterfaceId"
    <*> getT "state"
    <*> getT "origin"

routeTableAssociationSink :: MonadThrow m
    => Consumer Event m [RouteTableAssociation]
routeTableAssociationSink = itemsSet "associationSet" $ RouteTableAssociation
    <$> getT "routeTableAssociationId"
    <*> getT "routeTableId"
    <*> getT "subnetId"
    <*> getT "main"

------------------------------------------------------------
-- createRouteTable
------------------------------------------------------------
createRouteTable
    :: (MonadResource m, MonadBaseControl IO m)
    => Text
    -> EC2 m RouteTable
createRouteTable vid =
    ec2Query "CreateRouteTable" ["VpcId" |= vid] $
        element "routeTable" routeTableSink

------------------------------------------------------------
-- deleteRouteTable
------------------------------------------------------------
deleteRouteTable
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ RouteTableId
    -> EC2 m Bool
deleteRouteTable = ec2Delete "DeleteRouteTable" "RouteTableId"

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
    params = [ "RouteTableId" |= rtid
             , "SubnetId" |= sid
             ]

------------------------------------------------------------
-- disassociateRouteTable
------------------------------------------------------------
disassociateRouteTable
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ AssociationId
    -> EC2 m Bool -- ^ return
disassociateRouteTable aid =
    ec2Query "DisassociateRouteTable" ["AssociationId" |= aid]
        $ getT "return"

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
    params = [ "AssociationId" |= aid
             , "RouteTableId" |= rtid
             ]
