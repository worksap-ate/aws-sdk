{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Cloud.AWS.RDS.DBSnapshot
    ( describeDBSnapshots
    , createDBSnapshot
    , deleteDBSnapshot
    , copyDBSnapshot
    ) where

import Data.Text (Text)
import Control.Applicative
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)
import Cloud.AWS.Lib.Parser.Unordered (XmlElement, (.<), element)

import Cloud.AWS.Lib.Query

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
        , "MaxRecords" |=? maxRecords
        , "SnapshotType" |=? sType
        ]

sinkDBSnapshots
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m [DBSnapshot]
sinkDBSnapshots = elements "DBSnapshot" sinkDBSnapshot

sinkDBSnapshot
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m DBSnapshot
sinkDBSnapshot xml = DBSnapshot
    <$> xml .< "Port"
    <*> xml .< "OptionGroupName"
    <*> xml .< "Iops"
    <*> xml .< "Engine"
    <*> xml .< "Status"
    <*> xml .< "SnapshotType"
    <*> xml .< "LicenseModel"
    <*> xml .< "DBInstanceIdentifier"
    <*> xml .< "EngineVersion"
    <*> xml .< "DBSnapshotIdentifier"
    <*> xml .< "SnapshotCreateTime"
    <*> xml .< "VpcId"
    <*> xml .< "AvailabilityZone"
    <*> xml .< "InstanceCreateTime"
    <*> xml .< "AllocatedStorage"
    <*> xml .< "MasterUsername"

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
