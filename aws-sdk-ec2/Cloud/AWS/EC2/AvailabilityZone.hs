{-# LANGUAGE FlexibleContexts #-}

module Cloud.AWS.EC2.AvailabilityZone
    ( describeAvailabilityZones
    ) where

import Data.Text (Text)

import Data.Conduit
import Control.Applicative
import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)

import Cloud.AWS.Lib.Parser.Unordered ((.<))

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Query

describeAvailabilityZones
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ ZoneNames
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m AvailabilityZone)
describeAvailabilityZones zones filters =
    ec2QuerySource "DescribeAvailabilityZones" params path $
        itemConduit availabilityZoneInfo
  where
    params =
        [ "ZoneName" |.#= zones
        , filtersParam filters
        ]
    path = itemsPath "availabilityZoneInfo"
    availabilityZoneInfo xml =
        AvailabilityZone
        <$> xml .< "zoneName"
        <*> xml .< "zoneState"
        <*> xml .< "regionName"
        <*> itemsSet "messageSet" (.< "message") xml
