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

import AWS.EC2.Convert
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
        [ ArrayParams "SubnetId" subnets
        , FilterParams filters
        ]

subnetSink :: MonadThrow m
    => GLSink Event m Subnet
subnetSink = Subnet
    <$> getT "subnetId"
    <*> getF "state" subnetState
    <*> getT "vpcId"
    <*> getT "cidrBlock"
    <*> getF "availableIpAddressCount" textToInt
    <*> getT "availabilityZone"
    <*> resourceTagSink

------------------------------------------------------------
-- CreateSubnet
------------------------------------------------------------
createSubnet
    :: (MonadResource m, MonadBaseControl IO m)
    => CreateSubnetRequest
    -> EC2 m Subnet
createSubnet param =
    ec2Query "CreateSubnet" param' $
          element "subnet" subnetSink
  where
    param' = createSubnetParam param

createSubnetParam :: CreateSubnetRequest -> [QueryParam]
createSubnetParam (CreateSubnetRequest vid cidr zone) =
    [ ValueParam "VpcId" vid
    , ValueParam "CidrBlock" cidr
    ] ++ maybe [] (\a -> [ValueParam "AvailabilityZone" a]) zone

------------------------------------------------------------
-- DeleteSubnet
------------------------------------------------------------
deleteSubnet
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ SubnetId
    -> EC2 m Bool
deleteSubnet sid =
    ec2Query "DeleteSubnet" [ValueParam "SubnetId" sid]
        $ getF "return" textToBool
