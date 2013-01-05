{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.EC2.Subnets
    ( describeSubnets
    , createSubnet
    , deleteSubnet
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
-- DescribeSubnets
------------------------------------------------------------
describeSubnets
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ SubnetIds
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m Subnet)
describeSubnets subnets filters = do
    ec2QuerySource "DescribeSubnets" params $
        itemConduit "subnetSet" subnetSink
  where
    params =
        [ "SubnetId" |.#= subnets
        , filtersParam filters
        ]

subnetSink :: MonadThrow m
    => GLSink Event m Subnet
subnetSink = Subnet
    <$> getT "subnetId"
    <*> getT "state"
    <*> getT "vpcId"
    <*> getT "cidrBlock"
    <*> getT "availableIpAddressCount"
    <*> getT "availabilityZone"
    <*> getT "defaultForAz"
    <*> getT "mapPublicIpOnLaunch"
    <*> resourceTagSink

------------------------------------------------------------
-- CreateSubnet
------------------------------------------------------------
createSubnet
    :: (MonadResource m, MonadBaseControl IO m)
    => CreateSubnetRequest
    -> EC2 m Subnet
createSubnet param =
    ec2Query "CreateSubnet" params $
          element "subnet" subnetSink
  where
    params = createSubnetParams param

createSubnetParams :: CreateSubnetRequest -> [QueryParam]
createSubnetParams (CreateSubnetRequest vid cidr zone) =
    [ "VpcId" |= vid
    , "CidrBlock" |= toText cidr
    , "AvailabilityZone" |=? zone
    ]

------------------------------------------------------------
-- DeleteSubnet
------------------------------------------------------------
deleteSubnet
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ SubnetId
    -> EC2 m Bool
deleteSubnet = ec2Delete "DeleteSubnet" "SubnetId"
