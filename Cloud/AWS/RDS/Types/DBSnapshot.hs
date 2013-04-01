module Cloud.AWS.RDS.Types.DBSnapshot
    ( DBSnapshot(..)
    ) where

import Cloud.AWS.Lib.FromText (Text, UTCTime)

data DBSnapshot = DBSnapshot
    { dbSnapshotPort :: Int
    , dbSnapshotOptionGroupName :: Maybe Text
    , dbSnapshotIops :: Maybe Int
    , dbSnapshotEngine :: Text
    , dbSnapshotStatus :: Text
    , dbSnapshotType :: Text
    , dbSnapshotLicenseModel :: Text
    , dbSnapshotDBInstanceIdentifier :: Text
    , dbSnapshotEngineVersion :: Text
    , dbSnapshotIdentifier :: Text
    , dbSnapshotCreateTime :: Maybe UTCTime
    , dbSnapshotVpcId :: Maybe Text
    , dbSnapshotAvailabilityZone :: Text
    , dbSnapshotInstanceCreateTime :: UTCTime
    , dbSnapshotAllocatedStorage :: Int
    , dbSnapshotMasterUsername :: Text
    }
  deriving (Show, Eq)
