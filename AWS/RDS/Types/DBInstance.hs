module AWS.RDS.Types.DBInstance
    ( DBInstance(..)
    , VpcSecurityGroupMembership(..)
    , DBParameterGroupStatus(..)
    , DBSecurityGroupMembership(..)
    , Endpoint(..)
    , OptionGroupMembership(..)
    , PendingModifiedValue(..)
    ) where

import AWS.Lib.FromText (Text, UTCTime)
import AWS.RDS.Types.DBSubnetGroup (DBSubnetGroup)

data DBInstance = DBInstance
    { dbiIops :: Maybe Int
    , dbiBackupRetentionPeriod :: Int
    , dbiDBInstanceStatus :: Maybe Text
    , dbiMultiAZ :: Bool
    , dbiVpcSecurityGroups :: [VpcSecurityGroupMembership]
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
    , dbiPubliclyAccessible :: Bool
    , dbiSecurityGroups :: [DBSecurityGroupMembership]
    , dbiAutoMinorVersionUpgrade :: Bool
    , dbiDBName :: Maybe Text
    , dbiInstanceCreateTime :: Maybe UTCTime
    , dbiAllocatedStorage :: Int -- ^ storage size in gigabytes
    , dbiDBInstanceClass :: Text
    , dbiMasterUsername :: Text
    }
  deriving (Show, Eq)

data VpcSecurityGroupMembership = VpcSecurityGroupMembership
    { vpcSecurityGroupMembershipStatus :: Text
    , vpcSecurityGroupMembershipVpcSecurityGroupId :: Text
    }
  deriving (Show, Eq)

data DBParameterGroupStatus = DBParameterGroupStatus
    { dbpgsParameterApplyStatus :: Text
    , dbpgsDBParameterGroupName :: Text
    }
  deriving (Show, Eq)

data DBSecurityGroupMembership = DBSecurityGroupMembership
    { dbsgmStatus :: Text
    , dbsgmDBSecurityGroupName :: Text
    }
  deriving (Show, Eq)

data Endpoint = Endpoint
    { epPort :: Int
    , epAddress :: Text
    }
  deriving (Show, Eq)

data OptionGroupMembership = OptionGroupMembership
    { ogmOptionGroupName :: Text
    , ogmStatus :: Text
    }
  deriving (Show, Eq)

data PendingModifiedValue
    = PMVAllocatedStorage Int
    | PMVBackupRetentionPeriod Int
    | PMVDBInstanceClass Text
    | PMVEngineVersion Text
    | PMVIops Int
    | PMVMasterUserPassword Text
    | PMVMultiAZ Bool
    | PMVPort Int
  deriving (Show, Eq)
