{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.RDS.DBSnapshot
    ( describeDBSnapshots
    , createDBSnapshot
    , deleteDBSnapshot
    ) where

import Data.Text (Text)
import Data.Conduit
import Control.Applicative
import Data.XML.Types (Event(..))

import AWS.Util
import AWS.Lib.Query
import AWS.Lib.Parser

import AWS.RDS.Types
import AWS.RDS.Internal

describeDBSnapshots
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Text -- ^ DBInstanceIdentifier
    -> Maybe Text -- ^ DBSnapshotIdentifier
    -> Maybe Text -- ^ Marker
    -> Maybe Int -- ^ MaxRecords
    -> Maybe Text -- ^ SnapshotType
    -> RDS m [DBSnapshot]
describeDBSnapshots dbiid dbsid marker maxRecords sType =
    rdsQuery "DescribeDBSnapshots" params sinkDBSnapshots
  where
    params =
        [ "DBInstanceIdentifier" |=? dbiid
        , "DBSnapshotIdentifier" |=? dbsid
        , "Marker" |=? marker
        , "MaxRecords" |=? toText <$> maxRecords
        , "SnapshotType" |=? sType
        ]

sinkDBSnapshots
    :: MonadThrow m
    => GLSink Event m [DBSnapshot]
sinkDBSnapshots = elements "DBSnapshot" sinkDBSnapshot

sinkDBSnapshot
    :: MonadThrow m
    => GLSink Event m DBSnapshot
sinkDBSnapshot = DBSnapshot
    <$> getT "Port"
    <*> getT "Iops"
    <*> getT "Engine"
    <*> getT "Status"
    <*> getT "SnapshotType"
    <*> getT "LicenseModel"
    <*> getT "DBInstanceIdentifier"
    <*> getT "EngineVersion"
    <*> getT "DBSnapshotIdentifier"
    <*> getT "SnapshotCreateTime"
    <*> getT "VpcId"
    <*> getT "AvailabilityZone"
    <*> getT "InstanceCreateTime"
    <*> getT "AllocatedStorage"
    <*> getT "MasterUsername"

createDBSnapshot
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ DBInstanceIdentifier
    -> Text -- ^ DBSnapshotIdentifier
    -> RDS m DBSnapshot
createDBSnapshot dbiid dbsid =
    rdsQuery "CreateDBSnapshot" params $
        element "DBSnapshot" sinkDBSnapshot
  where
    params =
        [ "DBInstanceIdentifier" |= dbiid
        , "DBSnapshotIdentifier" |= dbsid
        ]

deleteDBSnapshot
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ DBSnapshotIdentifier
    -> RDS m DBSnapshot
deleteDBSnapshot dbsid =
    rdsQuery "DeleteDBSnapshot" params $
        element "DBSnapshot" sinkDBSnapshot
  where
    params = ["DBSnapshotIdentifier" |= dbsid]
