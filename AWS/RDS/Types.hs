module AWS.RDS.Types
    where

import Data.Text (Text)
import Data.Time (UTCTime)

data DBInstance = DBInstance
    { dbiIops :: Maybe Int
    , dbiBackupRetentionPeriod :: Int
    , dbiMultiAZ :: Bool
    , dbiDBInstanceStatus :: Text
    , dbiDBInstanceIdentifier :: Text
    , dbiPreferredBackupWindow :: Text
    , dbiPreferredMaintenanceWindow :: Text
    , dbiOptionGroupMembership :: Maybe OptionGroupMembership
    , dbiAvailabilityZone :: Text
    , dbiLatestRestorableTime :: Maybe UTCTime
    , dbiReadReplicaDBInstanceIdentifiers :: [Text]
    , dbiEngine :: Text
    , dbiPendingModifiedValues :: [PendingModifiedValue]
    , dbiCharacterSetName :: Maybe Text
    , dbiLicenseModel :: Text
    , dbiSubnetGroup :: Maybe DBSubnetGroup
    , dbiDBParameterGroups :: [DBParameterGroupStatus]
    , dbiEndpoint :: Maybe Endpoint
    , dbiEngineVersion :: Text
    , dbiReadReplicaSourceDBInstanceIdentifier :: Maybe Text
    , dbiSecurityGroups :: [DBSecurityGroupMembership]
    , dbiDBName :: Maybe Text
    , dbiAutoMinorVersionUpgrade :: Bool
    , dbiInstanceCreateTime :: Maybe UTCTime
    , dbiAllocatedStorage :: Int -- ^ storage size in gigabytes
    , dbiDBInstanceClass :: Text
    , dbiMasterUsername :: Text
    }
  deriving (Show)

data DBParameterGroupStatus = DBParameterGroupStatus
    { dbpgsParameterApplyStatus :: Text
    , dbpgsDBParameterGroupName :: Text
    }
  deriving (Show)

data DBSecurityGroupMembership = DBSecurityGroupMembership
    { dbsgmStatus :: Text
    , dbsgmDBSecurityGroupName :: Text
    }
  deriving (Show)

data DBSubnetGroup = DBSubnetGroup
    { dbsngVpcId :: Text
    , dbsngSubnetGroupStatus :: Text
    , dbsngDBSubnetGroupDescription :: Text
    , dbsngDBSubnetGroupName :: Text
    , dbsngSubnets :: [Subnet]
    }
  deriving (Show)

data Subnet = Subnet
    { subnetStatus :: Text
    , subnetIdentifier :: Text
    , subnetAvailabilityZone :: AvailabilityZone
    }
  deriving (Show)

data AvailabilityZone = AvailabilityZone
    { azName :: Text
    , azProvisionedIopsCapable :: Bool
    }
  deriving (Show)

data Endpoint = Endpoint
    { epPort :: Int
    , epAddress :: Text
    }
  deriving (Show)

data OptionGroupMembership = OptionGroupMembership
    { ogmOptionGroupName :: Text
    , ogmStatus :: Text
    }
  deriving (Show)

data PendingModifiedValue
    = PMVAllocatedStorage Int
    | PMVBackupRetentionPeriod Int
    | PMVDBInstanceClass Text
    | PMVEngineVersion Text
    | PMVIops Int
    | PMVMasterUserPassword Text
    | PMVMultiAZ Bool
    | PMVPort Int
  deriving (Show)
