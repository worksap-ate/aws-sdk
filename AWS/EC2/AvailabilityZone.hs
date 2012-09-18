{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.AvailabilityZone
    ( describeAvailabilityZones
    ) where

import           Data.ByteString (ByteString)

import Data.XML.Types (Event)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative

import AWS.EC2.Types
import AWS.EC2.Query
import AWS.EC2.Parser

describeAvailabilityZones
    :: (MonadResource m, MonadBaseControl IO m)
    => [ByteString]
    -> [Filter]
    -> EC2 m (EC2Response (Source m AvailabilityZone))
describeAvailabilityZones zones filters =
    ec2Query "DescribeAvailabilityZones" params availabilityZoneInfo
  where
    params =
        [ ArrayParams "ZoneName" zones
        , FilterParams filters
        ]
    availabilityZoneInfo :: MonadThrow m
        => GLConduit Event m AvailabilityZone
    availabilityZoneInfo = itemConduit "availabilityZoneInfo" $
        availabilityZone
        <$> getT "zoneName"
        <*> getT "zoneState"
        <*> getT "regionName"
        <*> itemsSet "messageSet" (getT "message")
