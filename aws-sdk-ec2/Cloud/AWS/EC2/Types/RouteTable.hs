{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.EC2.Types.RouteTable
    ( PropagatingVgw
    , Route(..)
    , RouteOrigin(..)
    , RouteState(..)
    , RouteTable(..)
    , RouteTableAssociation(..)
    ) where

import Cloud.AWS.EC2.Types.Common (ResourceTag)
import Cloud.AWS.Lib.FromText (deriveFromText)
import Data.Text (Text)

type PropagatingVgw = Text

data Route = Route
    { routeDestinationCidrBlock :: Text
    , routeGatewayId :: Maybe Text
    , routeInstanceId :: Maybe Text
    , routeInstanceOwnerId :: Maybe Text
    , routeNetworkInterfaceId :: Maybe Text
    , routeState :: RouteState
    , routeOrigin :: Maybe RouteOrigin
    }
  deriving (Show, Read, Eq)

data RouteOrigin
    = RouteOriginCreateRouteTable
    | RouteOriginCreateRoute
    | RouteOriginTableEnableVgwRoutePropagation
  deriving (Show, Read, Eq)

data RouteState
    = RouteStateActive
    | RouteStateBlackhole
  deriving (Show, Read, Eq)

data RouteTable = RouteTable
    { routeTableId :: Text
    , routeTableVpcId :: Text
    , routeTableRouteSet :: [Route]
    , routeTableAssociationSet :: [RouteTableAssociation]
    , routeTablePropagatingVgw :: Maybe PropagatingVgw
    , routeTableTagSet :: [ResourceTag]
    }
  deriving (Show, Read, Eq)

data RouteTableAssociation = RouteTableAssociation
    { routeTableAssociationId :: Text
    , routeTableAssociationRouteTableId :: Text
    , routeTableAssociationSubnetId :: Maybe Text
    , routeTableAssociationMain :: Maybe Bool
    }
  deriving (Show, Read, Eq)

deriveFromText "RouteOrigin"
    [ "CreateRouteTable"
    , "CreateRoute"
    , "EnableVgwRoutePropagation"
    ]

deriveFromText "RouteState" ["active", "blackhole"]
