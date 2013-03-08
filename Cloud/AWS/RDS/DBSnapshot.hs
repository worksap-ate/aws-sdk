{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Cloud.AWS.RDS.DBSnapshot
    ( describeDBSnapshots
    , createDBSnapshot
    , deleteDBSnapshot
    , copyDBSnapshot
    ) where

import Data.Text (Text)
import Data.Conduit
import Control.Applicative
import Data.XML.Types (Event(..))

import Cloud.AWS.Util
import Cloud.AWS.Lib.Query
import Cloud.AWS.Lib.Parser

import Cloud.AWS.RDS.Types hiding (Event)
import Cloud.AWS.RDS.Internal

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
    => Consumer Event m [DBSnapshot]
sinkDBSnapshots = elements "DBSnapshot" sinkDBSnapshot

sinkDBSnapshot
    :: MonadThrow m
    => Consumer Event m DBSnapshot
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

copyDBSnapshot
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ SourceDBSnapshotIdentifier
    -> Text -- ^ TargetDBSnapshotIdentifier
    -> RDS m DBSnapshot
copyDBSnapshot source target =
    rdsQuery "CopyDBSnapshot" params $
        element "DBSnapshot" sinkDBSnapshot
  where
    params =
        [ "SourceDBSnapshotIdentifier" |= source
        , "TargetDBSnapshotIdentifier" |= target
        ]
