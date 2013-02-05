{-# LANGUAGE TemplateHaskell #-}

module AWS.RDS.Types.DBInstance
    ( DBInstance(..)
    , VpcSecurityGroupMembership(..)
    , DBParameterGroupStatus(..)
    , DBSecurityGroupMembership(..)
    , Endpoint(..)
    , OptionGroupMembership(..)
    , PendingModifiedValue(..)
    , CreateDBInstanceRequest(..)
    , DBInstanceClass(..)
    , Engine(..)
    , LicenseModel(..)
    , FinalSnapshot(..)
    ) where

import AWS.Lib.FromText (Text, UTCTime, deriveFromText)
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
    , dbiAvailabilityZone :: Maybe Text
    , dbiLatestRestorableTime :: Maybe UTCTime
    , dbiReadReplicaDBInstanceIdentifiers :: [Text]
    , dbiEngine :: Engine
    , dbiPendingModifiedValues :: [PendingModifiedValue]
    , dbiCharacterSetName :: Maybe Text
    , dbiLicenseModel :: LicenseModel
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
    , dbiDBInstanceClass :: DBInstanceClass
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

data CreateDBInstanceRequest = CreateDBInstanceRequest
    { createDBInstanceAllocatedStorage :: Int
    , createDBInstanceAutoMinorVersionUpgrade :: Maybe Bool
    , createDBInstanceAvailabilityZone :: Maybe Text
    , createDBInstanceBackupRetentionPeriod :: Maybe Int
    , createDBInstanceCharacterSetName :: Maybe Text
    , createDBInstanceDBInstanceClass :: DBInstanceClass
    , createDBInstanceDBInstanceIdentifier :: Text
    , createDBInstanceDBName :: Maybe Text
    , createDBInstanceDBParameterGroupName :: Maybe Text
    , createDBInstanceDBSecurityGroups :: [Text]
    , createDBInstanceDBSubnetGroupName :: Maybe Text
    , createDBInstanceEngine :: Engine
    , createDBInstanceEngineVersion :: Maybe Text
    , createDBInstanceIops :: Maybe Int
    , createDBInstanceLicenseModel :: Maybe LicenseModel
    , createDBInstanceMasterUserPassword :: Text
    , createDBInstanceMasterUsername :: Text
    , createDBInstanceMultiAZ :: Maybe Bool
    , createDBInstanceOptionGroupName :: Maybe Text
    , createDBInstancePort :: Maybe Int
    , createDBInstancePreferredBackupWindow :: Maybe Text
    , createDBInstancePreferredMaintenanceWindow :: Maybe Text
    , createDBInstancePubliclyAccessible :: Maybe Bool
    , createDBInstanceVpcSecurityGroupIds :: [Text]
    }
  deriving (Show, Eq)

data DBInstanceClass
    = DBt1micro
    | DBm1small
    | DBm1medium
    | DBm1large
    | DBm1xlarge
    | DBm2xlarge
    | DBm22xlarge
    | DBm24xlarge
  deriving (Read, Eq)

instance Show DBInstanceClass where
    show DBt1micro = "db.t1.micro"
    show DBm1small = "db.m1.small"
    show DBm1medium = "db.m1.medium"
    show DBm1large = "db.m1.large"
    show DBm1xlarge = "db.m1.xlarge"
    show DBm2xlarge = "db.m2.xlarge"
    show DBm22xlarge = "db.m2.2xlarge"
    show DBm24xlarge = "db.m2.4xlarge"

data Engine
    = EngineMySQL
    | EngineOracleSE1
    | EngineOracleSE
    | EngineOracleEE
    | EngineSqlServerEE
    | EngineSqlServerSE
    | EngineSqlServerEX
    | EngineSqlServerWeb
  deriving (Read, Eq)

instance Show Engine where
    show EngineMySQL = "MySQL"
    show EngineOracleSE1 = "oracle-se1"
    show EngineOracleSE = "oracle-se"
    show EngineOracleEE = "oracle-ee"
    show EngineSqlServerEE = "sqlserver-ee"
    show EngineSqlServerSE = "sqlserver-se"
    show EngineSqlServerEX = "sqlserver-ex"
    show EngineSqlServerWeb = "sqlserver-web"

data LicenseModel
    = LicenseIncluded
    | BringYourOwnLicense
    | GeneralPublicLicense
  deriving (Read, Eq)

instance Show LicenseModel where
    show LicenseIncluded = "license-included"
    show BringYourOwnLicense = "bring-your-own-license"
    show GeneralPublicLicense = "general-public-license"

data FinalSnapshot
    = FinalSnapshotIdentifier Text
    | SkipFinalSnapshot

deriveFromText "DBInstanceClass"
    [ "db.t1.micro", "db.m1.small", "db.m1.medium"
    , "db.m1.large", "db.m1.xlarge", "db.m2.xlarge"
    , "db.m2.2xlarge", "db.m2.4xlarge"
    ]
deriveFromText "Engine"
    [ "mysql", "oracle-se1", "oracle-se"
    , "oracle-ee", "sqlserver-ee", "sqlserver-se"
    , "sqlserver-ex", "sqlserver-web"
    ]
deriveFromText "LicenseModel"
    [ "license-included", "bring-your-own-license", "general-public-license"
    ]
