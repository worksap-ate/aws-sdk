{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.AvailabilityZone
    ( describeAvailabilityZones
    ) where

import Data.Text (Text)

import Data.XML.Types (Event)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative

import AWS.EC2.Types
import AWS.EC2.Class
import AWS.EC2.Query
import AWS.EC2.Parser

describeAvailabilityZones
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ ZoneNames
    -> [Filter] -- ^ Filters
    -> EC2 m (Source m AvailabilityZone)
describeAvailabilityZones zones filters =
    ec2QuerySource "DescribeAvailabilityZones" params availabilityZoneInfo
  where
    params =
        [ ArrayParams "ZoneName" zones
        , FilterParams filters
        ]
    availabilityZoneInfo :: MonadThrow m
        => GLConduit Event m AvailabilityZone
    availabilityZoneInfo = itemConduit "availabilityZoneInfo" $
        AvailabilityZone
        <$> getT "zoneName"
        <*> getT "zoneState"
        <*> getT "regionName"
        <*> itemsSet "messageSet" (getT "message")
