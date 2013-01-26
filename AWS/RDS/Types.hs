{-# LANGUAGE TemplateHaskell #-}
module AWS.RDS.Types
    where

import AWS.Lib.FromText

data DBInstance = DBInstance
    { dbiIops :: Maybe Int
    , dbiBackupRetentionPeriod :: Int
    , dbiDBInstanceStatus :: Text
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

data DBSubnetGroup = DBSubnetGroup
    { dbsngVpcId :: Text
    , dbsngSubnetGroupStatus :: Text
    , dbsngDBSubnetGroupDescription :: Text
    , dbsngDBSubnetGroupName :: Text
    , dbsngSubnets :: [Subnet]
    }
  deriving (Show, Eq)

data Subnet = Subnet
    { subnetStatus :: Text
    , subnetIdentifier :: Text
    , subnetAvailabilityZone :: AvailabilityZone
    }
  deriving (Show, Eq)

data AvailabilityZone = AvailabilityZone
    { azName :: Text
    , azProvisionedIopsCapable :: Bool
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

data Event = Event
    { eventMessage :: Text
    , eventSourceType :: SourceType
    , eventEventCategories :: [Text]
    , eventDate :: UTCTime
    , eventSourceIdentifier :: Text
    }
  deriving (Show, Eq)

data SourceType
    = SourceTypeDBInstance
    | SourceTypeDBParameterGroup
    | SourceTypeDBSecurityGroup
    | SourceTypeDBSnapshot
  deriving (Show, Read, Eq)

deriveFromText "SourceType" ["db-instance", "db-parameter-group", "db-security-group", "db-snapshot"]
