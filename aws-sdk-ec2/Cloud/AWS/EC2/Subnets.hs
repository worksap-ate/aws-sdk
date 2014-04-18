{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Cloud.AWS.EC2.Subnets
    ( describeSubnets
    , createSubnet
    , deleteSubnet
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
-- DescribeSubnets
------------------------------------------------------------
describeSubnets
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ SubnetIds
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m Subnet)
describeSubnets subnets filters = do
    ec2QuerySource "DescribeSubnets" params path $
        itemConduit subnetConv
  where
    path = itemsPath "subnetSet"
    params =
        [ "SubnetId" |.#= subnets
        , filtersParam filters
        ]

subnetConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m Subnet
subnetConv xml = Subnet
    <$> xml .< "subnetId"
    <*> xml .< "state"
    <*> xml .< "vpcId"
    <*> xml .< "cidrBlock"
    <*> xml .< "availableIpAddressCount"
    <*> xml .< "availabilityZone"
    <*> xml .< "defaultForAz"
    <*> xml .< "mapPublicIpOnLaunch"
    <*> resourceTagConv xml

------------------------------------------------------------
-- CreateSubnet
------------------------------------------------------------
createSubnet
    :: (MonadResource m, MonadBaseControl IO m)
    => CreateSubnetRequest
    -> EC2 m Subnet
createSubnet param =
    ec2Query "CreateSubnet" params $
        element "subnet" subnetConv
  where
    params = createSubnetParams param

createSubnetParams :: CreateSubnetRequest -> [QueryParam]
createSubnetParams (CreateSubnetRequest vid cidr zone) =
    [ "VpcId" |= vid
    , "CidrBlock" |= cidr
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
