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

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Query
import Cloud.AWS.Lib.Parser.Unordered

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
        itemConduit' "routeTableSet" routeTableConv
  where
    params =
        [ "RouteTableId" |.#= routeTables
        , filtersParam filters
        ]

routeTableConv :: (MonadThrow m, Applicative m)
    => SimpleXML -> m RouteTable
routeTableConv xml = RouteTable
    <$> xml .< "routeTableId"
    <*> xml .< "vpcId"
    <*> routeConv xml
    <*> routeTableAssociationConv xml
    <*> xml .< "propagatingVgwSet"
    <*> resourceTagConv xml

routeConv :: (MonadThrow m, Applicative m)
    => SimpleXML -> m [Route]
routeConv xml = itemsSet' xml "routeSet" $ \xml' -> Route
    <$> xml' .< "destinationCidrBlock"
    <*> xml' .< "gatewayId"
    <*> xml' .< "instanceId"
    <*> xml' .< "instanceOwnerId"
    <*> xml' .< "networkInterfaceId"
    <*> xml' .< "state"
    <*> xml' .< "origin"

routeTableAssociationConv :: (MonadThrow m, Applicative m)
    => SimpleXML -> m [RouteTableAssociation]
routeTableAssociationConv xml = itemsSet' xml "associationSet" $ \xml' ->
    RouteTableAssociation
    <$> xml' .< "routeTableAssociationId"
    <*> xml' .< "routeTableId"
    <*> xml' .< "subnetId"
    <*> xml' .< "main"

------------------------------------------------------------
-- createRouteTable
------------------------------------------------------------
createRouteTable
    :: (MonadResource m, MonadBaseControl IO m)
    => Text
    -> EC2 m RouteTable
createRouteTable vid =
    ec2Query "CreateRouteTable" ["VpcId" |= vid] $ xmlParser $ \xml ->
        getElement xml "routeTable" routeTableConv

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
        $ xmlParser (.< "associationId")
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
        $ xmlParser (.< "return")

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
        $ xmlParser (.< "newAssociationId")
  where
    params = [ "AssociationId" |= aid
             , "RouteTableId" |= rtid
             ]
