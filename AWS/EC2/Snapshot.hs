{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Snapshot
    ( describeSnapshots
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
import AWS.Util

describeSnapshots
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ SnapshotIds
    -> [Text] -- ^ Owners (UserId)
    -> [Text] -- ^ RestorableBy (UserId)
    -> [Filter] -- ^ Filters
    -> EC2 m (Source m Snapshot)
describeSnapshots ssids owners restby filters =
    ec2QuerySource "DescribeSnapshots" params snapshotSet
  where
    params =
        [ ArrayParams "SnapshotId" ssids
        , ArrayParams "Owner" owners
        , ArrayParams "RestorableBy" restby
        , FilterParams filters
        ]
    snapshotSet :: MonadThrow m
        => GLConduit Event m Snapshot
    snapshotSet = itemConduit "snapshotSet" $
        snapshot
        <$> getT "snapshotId"
        <*> getT "volumeId"
        <*> getF "status" snapshotStatus
        <*> getF "startTime" textToTime
        <*> getT "progress"
        <*> getT "ownerId"
        <*> getF "volumeSize" textToInt
        <*> getT "description"
        <*> getMT "ownerAlias"
        <*> resourceTagSink
