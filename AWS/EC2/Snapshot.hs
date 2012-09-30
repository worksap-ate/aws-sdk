{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Snapshot
    ( describeSnapshots
    , createSnapshot
    , deleteSnapshot
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
    ec2QuerySource "DescribeSnapshots" params $
        itemConduit "snapshotSet" snapshotSink
  where
    params =
        [ ArrayParams "SnapshotId" ssids
        , ArrayParams "Owner" owners
        , ArrayParams "RestorableBy" restby
        , FilterParams filters
        ]

snapshotSink :: MonadThrow m
    => GLSink Event m Snapshot
snapshotSink = Snapshot
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

createSnapshot
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ SnapshotId
    -> Maybe Text -- ^ Description
    -> EC2 m Snapshot
createSnapshot volid desc =
    ec2Query "CreateSnapshot" params snapshotSink
  where
    params = [ValueParam "VolumeId" volid]
        ++ maybe [] (\a -> [ValueParam "Description" a]) desc

deleteSnapshot
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ SnapshotId
    -> EC2 m Bool
deleteSnapshot ssid =
    ec2Query "DeleteSnapshot" params $ getF "return" textToBool
  where
    params = [ValueParam "SnapshotId" ssid]
