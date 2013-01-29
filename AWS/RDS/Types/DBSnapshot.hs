module AWS.RDS.Types.DBSnapshot
    ( DBSnapshot(..)
    ) where

import AWS.Lib.FromText (Text, UTCTime)

data DBSnapshot = DBSnapshot
    { dbsPort :: Int
    , dbsIops :: Maybe Int
    , dbsEngine :: Text
    , dbsStatus :: Text
    , dbsSnapshotType :: Text
    , dbsLicenseModel :: Text
    , dbsDBInstanceIdentifier :: Text
    , dbsEngineVersion :: Text
    , dbsDBSnapshotIdentifier :: Text
    , dbsSnapshotCreateTime :: Maybe UTCTime
    , dbsVpcId :: Maybe Text
    , dbsAvailabilityZone :: Text
    , dbsInstanceCreateTime :: UTCTime
    , dbsAllocatedStorage :: Int
    , dbsMasterUsername :: Text
    }
  deriving (Show, Eq)
