{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.RDS.Types.DBInstance
    ( DBInstance(..)
    , VpcSecurityGroupMembership(..)
    , DBParameterGroupStatus(..)
    , DBSecurityGroupMembership(..)
    , Endpoint(..)
    , OptionGroupMembership(..)
    , PendingModifiedValue(..)
    , CreateDBInstanceRequest(..)
    , DBInstanceClass
    , Engine
    , LicenseModel(..)
    , FinalSnapshot(..)
    , CreateReadReplicaRequest(..)
    , RestoreDBInstanceFromDBSnapshotRequest(..)
    , ModifyDBInstanceRequest(..)
    , OrderableDBInstanceOption(..)
    , RestoreDBInstanceToPointInTimeRequest(..)
    , RestoreTime(..)
    , ReservedDBInstancesOffering(..)
    , RecurringCharge(..)
    ) where

import Cloud.AWS.Lib.FromText (deriveFromText)
import Cloud.AWS.Lib.ToText (deriveToText)
import Cloud.AWS.RDS.Types.DBSubnetGroup (DBSubnetGroup, AvailabilityZone)
import Data.Text (Text)
import Data.Time (UTCTime)

data DBInstance = DBInstance
    { dbInstanceIops :: Maybe Int
    , dbInstanceBackupRetentionPeriod :: Int
    , dbInstanceStatus :: Maybe Text
    , dbInstanceMultiAZ :: Bool
    , dbInstanceVpcSecurityGroups :: [VpcSecurityGroupMembership]
    , dbInstanceIdentifier :: Text
    , dbInstancePreferredBackupWindow :: Text
    , dbInstancePreferredMaintenanceWindow :: Text
    , dbInstanceAvailabilityZone :: Maybe Text
    , dbInstanceLatestRestorableTime :: Maybe UTCTime
    , dbInstanceReadReplicaDBInstanceIdentifiers :: [Text]
    , dbInstanceEngine :: Engine
    , dbInstancePendingModifiedValues :: [PendingModifiedValue]
    , dbInstanceCharacterSetName :: Maybe Text
    , dbInstanceLicenseModel :: LicenseModel
    , dbInstanceSubnetGroup :: Maybe DBSubnetGroup
    , dbInstanceDBParameterGroups :: [DBParameterGroupStatus]
    , dbInstanceEndpoint :: Maybe Endpoint
    , dbInstanceEngineVersion :: Text
    , dbInstanceReadReplicaSourceDBInstanceIdentifier :: Maybe Text
    , dbInstanceOptionGroupMemberships :: [OptionGroupMembership]
    , dbInstancePubliclyAccessible :: Bool
    , dbInstanceSecurityGroups :: [DBSecurityGroupMembership]
    , dbInstanceAutoMinorVersionUpgrade :: Bool
    , dbInstanceDBName :: Maybe Text
    , dbInstanceCreateTime :: Maybe UTCTime
    , dbInstanceAllocatedStorage :: Int -- ^ storage size in gigabytes
    , dbInstanceClass :: DBInstanceClass
    , dbInstanceMasterUsername :: Text
    }
  deriving (Show, Eq)

data VpcSecurityGroupMembership = VpcSecurityGroupMembership
    { vpcSecurityGroupStatus :: Text
    , vpcSecurityGroupId :: Text
    }
  deriving (Show, Eq)

data DBParameterGroupStatus = DBParameterGroupStatus
    { dbParameterGroupStatusStatus :: Text
    , dbParameterGroupStatusName :: Text
    }
  deriving (Show, Eq)

data DBSecurityGroupMembership = DBSecurityGroupMembership
    { dbSecurityGroupMembershipStatus :: Text
    , dbSecurityGroupMembershipName :: Text
    }
  deriving (Show, Eq)

data Endpoint = Endpoint
    { endpointPort :: Int
    , endpointAddress :: Maybe Text
    }
  deriving (Show, Eq)

data OptionGroupMembership = OptionGroupMembership
    { optionGroupMembershipName :: Text
    , optionGroupMembershipStatus :: Text
    }
  deriving (Show, Eq)

data PendingModifiedValue
    = PendingModifiedValueAllocatedStorage Int
    | PendingModifiedValueBackupRetentionPeriod Int
    | PendingModifiedValueDBInstanceClass Text
    | PendingModifiedValueEngineVersion Text
    | PendingModifiedValueIops Int
    | PendingModifiedValueMasterUserPassword Text
    | PendingModifiedValueMultiAZ Bool
    | PendingModifiedValuePort Int
    | PendingModifiedValueDBInstanceIdentifier Text
  deriving (Show, Eq)

data CreateDBInstanceRequest = CreateDBInstanceRequest
    { createDBInstanceAllocatedStorage :: Int
    , createDBInstanceAutoMinorVersionUpgrade :: Maybe Bool
    , createDBInstanceAvailabilityZone :: Maybe Text
    , createDBInstanceBackupRetentionPeriod :: Maybe Int
    , createDBInstanceCharacterSetName :: Maybe Text
    , createDBInstanceClass :: DBInstanceClass
    , createDBInstanceIdentifier :: Text
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

-- | db.t1.micro, db.m1.small, db.m1.medium, db.m1.large,
--   db.m1.xlarge, db.m2.xlarge, db.m2.2xlarge, db.m2.4xlarge
type DBInstanceClass = Text

-- | mysql, oracle-se1, oracle-se, oracle-ee, sqlserver-ee,
--   sqlserver-se, sqlserver-ex, sqlserver-web
type Engine = Text

data LicenseModel
    = LicenseIncluded
    | BringYourOwnLicense
    | GeneralPublicLicense
  deriving (Read, Eq, Show)

deriveToText "LicenseModel"
    [ "license-included"
    , "bring-your-own-license"
    , "general-public-license"
    ]

data FinalSnapshot
    = FinalSnapshotIdentifier Text
    | SkipFinalSnapshot

data CreateReadReplicaRequest = CreateReadReplicaRequest
    { createReadReplicaAutoMinorVersionUpgrade :: Maybe Bool
    , createReadReplicaAvailabilityZone :: Maybe Text
    , createReadReplicaDBInstanceClass :: DBInstanceClass
    , createReadReplicaDBInstanceIdentifier :: Text
    , createReadReplicaIops :: Maybe Int
    , createReadReplicaOptionGroupName :: Maybe Text
    , createReadReplicaPort :: Maybe Int
    , createReadReplicaPubliclyAccessible :: Maybe Bool
    , createReadReplicaSourceDBInstanceIdentifier :: Text
    }
  deriving (Show, Eq)

data RestoreDBInstanceFromDBSnapshotRequest
    = RestoreDBInstanceFromDBSnapshotRequest
        { restoreDBInstanceFromDBSnapshotRequestAutoMinorVersionUpgrade
            :: Maybe Bool
        , restoreDBInstanceFromDBSnapshotRequestAvailabilityZone
            :: Maybe Text
        , restoreDBInstanceFromDBSnapshotRequestDBInstanceClass
            :: Maybe DBInstanceClass
        , restoreDBInstanceFromDBSnapshotRequestDBInstanceIdentifier
            :: Text
        , restoreDBInstanceFromDBSnapshotRequestDBName
            :: Maybe Text
        , restoreDBInstanceFromDBSnapshotRequestDBSnapshotIdentifier
            :: Text
        , restoreDBInstanceFromDBSnapshotRequestDBSubnetGroupName
            :: Maybe Text
        , restoreDBInstanceFromDBSnapshotRequestEngine
            :: Maybe Engine
        , restoreDBInstanceFromDBSnapshotRequestIops
            :: Maybe Int
        , restoreDBInstanceFromDBSnapshotRequestLicenseModel
            :: Maybe LicenseModel
        , restoreDBInstanceFromDBSnapshotRequestMultiAZ
            :: Maybe Bool
        , restoreDBInstanceFromDBSnapshotRequestOptionGroupName
            :: Maybe Text
        , restoreDBInstanceFromDBSnapshotRequestPort
            :: Maybe Int
        , restoreDBInstanceFromDBSnapshotRequestPubliclyAccessible
            :: Maybe Bool
        }
  deriving (Show, Eq)

data ModifyDBInstanceRequest = ModifyDBInstanceRequest
    { modifyDBInstanceRequestAllocatedStorage :: Maybe Int
    , modifyDBInstanceRequestAllowMajorVersionUpgrade :: Maybe Bool
    , modifyDBInstanceRequestApplyImmediately :: Maybe Bool
    , modifyDBInstanceRequestAutoMinorVersionUpgrade :: Maybe Bool
    , modifyDBInstanceRequestBackupRetentionPeriod :: Maybe Int
    , modifyDBInstanceRequestDBInstanceClass :: Maybe DBInstanceClass
    , modifyDBInstanceRequestDBInstanceIdentifier :: Text
    , modifyDBInstanceRequestDBParameterGroupName :: Maybe Text
    , modifyDBInstanceRequestDBSecurityGroups :: [Text]
    , modifyDBInstanceRequestEngineVersion :: Maybe Text
    , modifyDBInstanceRequestIops :: Maybe Int
    , modifyDBInstanceRequestMasterUserPassword :: Maybe Text
    , modifyDBInstanceRequestMultiAZ :: Maybe Bool
    , modifyDBInstanceRequestNewDBInstanceIdentifier :: Maybe Text
    , modifyDBInstanceRequestOptionGroupName :: Maybe Text
    , modifyDBInstanceRequestPrefferedBackupWindow :: Maybe Text
    , modifyDBInstanceRequestPrefferedMaintenanceWindow :: Maybe Text
    , modifyDBInstanceRequestVpcSecurityGroupIds :: [Text]
    }
  deriving (Show, Eq)

data OrderableDBInstanceOption = OrderableDBInstanceOption
    { orderableDBInstanceOptionMultiAZCapable :: Bool
    , orderableDBInstanceOptionEngine :: Engine
    , orderableDBInstanceOptionLicenseModel :: LicenseModel
    , orderableDBInstanceOptionReadReplicaCapable :: Bool
    , orderableDBInstanceOptionVpc :: Bool
    , orderableDBInstanceOptionEngineVersion :: Text
    , orderableDBInstanceOptionAvailabilityZones :: [AvailabilityZone]
    , orderableDBInstanceOptionDBInstanceClass :: DBInstanceClass
    }
  deriving (Show, Eq)

data RestoreDBInstanceToPointInTimeRequest
    = RestoreDBInstanceToPointInTimeRequest
        { restoreDBInstanceToPointInTimeSourceDBInstanceIdentifier
            :: Text
        , restoreDBInstanceToPointInTimeTargetDBInstanceIdentifier
            :: Text
        , restoreDBInstanceToPointInTimeAutoMinorVersionUpgrade
            :: Maybe Bool
        , restoreDBInstanceToPointInTimeAvailabilityZone
            :: Maybe Text
        , restoreDBInstanceToPointInTimeDBInstanceClass
            :: Maybe DBInstanceClass
        , restoreDBInstanceToPointInTimeDBName
            :: Maybe Text
        , restoreDBInstanceToPointInTimeDBSubnetGroupName
            :: Maybe Text
        , restoreDBInstanceToPointInTimeEngine
            :: Maybe Engine
        , restoreDBInstanceToPointInTimeIops
            :: Maybe Int
        , restoreDBInstanceToPointInTimeLicenseModel
            :: Maybe LicenseModel
        , restoreDBInstanceToPointInTimeMultiAZ
            :: Maybe Bool
        , restoreDBInstanceToPointInTimeOptionGroupName
            :: Maybe Text
        , restoreDBInstanceToPointInTimePort
            :: Maybe Int
        , restoreDBInstanceToPointInTimePubliclyAccessible
            :: Maybe Bool
        }
  deriving (Show, Eq)

data RestoreTime
    = UseLatestRestorableTime
    | RestoreTime UTCTime
  deriving (Show, Eq)

data ReservedDBInstancesOffering = ReservedDBInstancesOffering
    { reservedDBInstanceOfferingOfferingType :: Text
    , reservedDBInstanceOfferingDuration ::  Int
    , reservedDBInstanceOfferingCurrencyCode :: Text
    , reservedDBInstanceOfferingRecurringCharge :: [RecurringCharge]
    , reservedDBInstanceOfferingFixedPrice :: Double
    , reservedDBInstanceOfferingProductDescription :: Text
    , reservedDBInstanceOfferingUsagePrice :: Double
    , reservedDBInstanceOfferingId :: Text
    , reservedDBInstanceOfferingMultiAZ :: Bool
    , reservedDBInstanceOfferingDBInstanceClass :: DBInstanceClass
    }
  deriving (Show, Eq)

data RecurringCharge = RecurringCharge
    { recurringChargeFrequency :: Text
    , recurringChargeAmount :: Double
    }
  deriving (Show, Eq)

deriveFromText "LicenseModel"
    [ "license-included", "bring-your-own-license", "general-public-license"
    ]
