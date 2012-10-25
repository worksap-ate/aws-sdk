{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.EC2.Subnets
    ( describeSubnets
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
    -> EC2 m (Source m Subnet)
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
