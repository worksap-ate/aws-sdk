{-# LANGUAGE FlexibleContexts #-}

module Cloud.AWS.EC2.AvailabilityZone
    ( describeAvailabilityZones
    ) where

import Data.Text (Text)

import Data.Conduit
import Control.Applicative

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Query
import Cloud.AWS.Lib.Parser.Unordered

describeAvailabilityZones
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ ZoneNames
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m AvailabilityZone)
describeAvailabilityZones zones filters =
    ec2QuerySource "DescribeAvailabilityZones" params $
        itemConduit' "availabilityZoneInfo" availabilityZoneInfo
  where
    params =
        [ "ZoneName" |.#= zones
        , filtersParam filters
        ]
    availabilityZoneInfo :: (MonadThrow m, Applicative m)
        => SimpleXML -> m AvailabilityZone
    availabilityZoneInfo xml =
        AvailabilityZone
        <$> xml .< "zoneName"
        <*> xml .< "zoneState"
        <*> xml .< "regionName"
        <*> itemsSet' xml "messageSet" (.< "message")
