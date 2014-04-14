{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Cloud.AWS.EC2.RouteTable
    ( associateRouteTable
    , createRouteTable
    , deleteRouteTable
    , describeRouteTables
    , disassociateRouteTable
    , replaceRouteTableAssociation
    ) where

import Data.Text (Text)
import Data.Conduit
import Control.Applicative
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)

import Cloud.AWS.Lib.Parser.Unordered (XmlElement, element, (.<))

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Query

------------------------------------------------------------
-- describeRouteTables
------------------------------------------------------------
describeRouteTables
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ RouteTableIds
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m RouteTable)
describeRouteTables routeTables filters = do
    ec2QuerySource "DescribeRouteTables" params path $
        itemConduit routeTableConv
  where
    path = itemsPath "routeTableSet"
    params =
        [ "RouteTableId" |.#= routeTables
        , filtersParam filters
        ]

routeTableConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m RouteTable
routeTableConv xml = RouteTable
    <$> xml .< "routeTableId"
    <*> xml .< "vpcId"
    <*> routeConv xml
    <*> routeTableAssociationConv xml
    <*> xml .< "propagatingVgwSet"
    <*> resourceTagConv xml

routeConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m [Route]
routeConv = itemsSet "routeSet" conv
  where
    conv e = Route
        <$> e .< "destinationCidrBlock"
        <*> e .< "gatewayId"
        <*> e .< "instanceId"
        <*> e .< "instanceOwnerId"
        <*> e .< "networkInterfaceId"
        <*> e .< "state"
        <*> e .< "origin"

routeTableAssociationConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m [RouteTableAssociation]
routeTableAssociationConv = itemsSet "associationSet" conv
  where
    conv e = RouteTableAssociation
        <$> e .< "routeTableAssociationId"
        <*> e .< "routeTableId"
        <*> e .< "subnetId"
        <*> e .< "main"

------------------------------------------------------------
-- createRouteTable
------------------------------------------------------------
createRouteTable
    :: (MonadResource m, MonadBaseControl IO m)
    => Text
    -> EC2 m RouteTable
createRouteTable vid =
    ec2Query "CreateRouteTable" ["VpcId" |= vid] $
        element "routeTable" routeTableConv

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
    ec2Query "AssociateRouteTable" params (.< "associationId")
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
        (.< "return")

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
        (.< "newAssociationId")
  where
    params = [ "AssociationId" |= aid
             , "RouteTableId" |= rtid
             ]
