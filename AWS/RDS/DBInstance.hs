{-# LANGUAGE FlexibleContexts, RankNTypes, RecordWildCards #-}

module AWS.RDS.DBInstance
    ( describeDBInstances
    , createDBInstance
    ) where

import Data.Text (Text)
import Data.Conduit
import Control.Applicative
import Data.XML.Types (Event(..))
import Data.Maybe (catMaybes)

import AWS.Util
import AWS.Lib.Query
import AWS.Lib.Parser

import AWS.RDS.Types hiding (Event)
import AWS.RDS.Internal

describeDBInstances
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Text -- ^ DBInstanceIdentifier
    -> Maybe Int -- ^ MaxRecords
    -> Maybe Text -- ^ Marker
    -> RDS m [DBInstance]
describeDBInstances dbid maxRecords marker =
    rdsQuery "DescribeDBInstances" params $
        elements "DBInstance" sinkDBInstance
  where
    params =
        [ "DBInstanceIdentifier" |=? dbid
        , "MaxRecords" |=? toText <$> maxRecords
        , "Marker" |=? marker
        ]

sinkDBInstance
    :: MonadThrow m
    => GLSink Event m DBInstance
sinkDBInstance = DBInstance
    <$> getT "Iops"
    <*> getT "BackupRetentionPeriod"
    <*> getT "DBInstanceStatus"
    <*> getT "MultiAZ"
    <*> elements' "VpcSecurityGroups" "VpcSecurityGroupMembership"
        (VpcSecurityGroupMembership
        <$> getT "Status"
        <*> getT "VpcSecurityGroupId"
        )
    <*> getT "DBInstanceIdentifier"
    <*> getT "PreferredBackupWindow"
    <*> getT "PreferredMaintenanceWindow"
    <*> elementM "OptionGroupMembership"
        (OptionGroupMembership
        <$> getT "OptionGroupName"
        <*> getT "Status"
        )
    <*> getT "AvailabilityZone"
    <*> getT "LatestRestorableTime"
    <*> elements "ReadReplicaDBInstanceIdentifier" text
    <*> getT "Engine"
    <*> sinkPendingModifiedValues
    <*> getT "CharacterSetName"
    <*> getT "LicenseModel"
    <*> elementM "DBSubnetGroup" dbSubnetGroupSink
    <*> elements "DBParameterGroup"
        (DBParameterGroupStatus
        <$> getT "ParameterApplyStatus"
        <*> getT "DBParameterGroupName"
        )
    <*> elementM "Endpoint"
        (Endpoint
        <$> getT "Port"
        <*> getT "Address"
        )
    <*> getT "EngineVersion"
    <*> getT "ReadReplicaSourceDBInstanceIdentifier"
    <*> getT "PubliclyAccessible"
    <*> elements "DBSecurityGroup"
        (DBSecurityGroupMembership
        <$> getT "Status"
        <*> getT "DBSecurityGroupName"
        )
    <*> getT "AutoMinorVersionUpgrade"
    <*> getT "DBName"
    <*> getT "InstanceCreateTime"
    <*> getT "AllocatedStorage"
    <*> getT "DBInstanceClass"
    <*> getT "MasterUsername"

sinkPendingModifiedValues
    :: MonadThrow m
    => GLSink Event m [PendingModifiedValue]
sinkPendingModifiedValues = element "PendingModifiedValues" $
    catMaybes <$> sequence
        [ f PMVMasterUserPassword "MasterUserPassword"
        , f PMVIops "Iops"
        , f PMVMultiAZ "MultiAZ"
        , f PMVAllocatedStorage "AllocatedStorage"
        , f PMVEngineVersion "EngineVersion"
        , f PMVDBInstanceClass "DBInstanceClass"
        , f PMVBackupRetentionPeriod "BackupRetentionPeriod"
        , f PMVPort "Port"
        ]
  where
    f c name = fmap c <$> getT name

createDBInstance
    :: (MonadBaseControl IO m, MonadResource m)
    => CreateDBInstanceRequest -- ^ data type of CreateDBInstance
    -> RDS m DBInstance
createDBInstance CreateDBInstanceRequest{..} =
    rdsQuery "CreateDBInstance" params $
        element "DBInstance" sinkDBInstance
  where
    params =
        [ "AllocatedStorage" |=
            toText createDBInstanceAllocatedStorage
        , "AutoMinorVersionUpgrade" |=?
            boolToText <$> createDBInstanceAutoMinorVersionUpgrade
        , "AvailabilityZone" |=?
            createDBInstanceAvailabilityZone
        , "BackupRetentionPeriod" |=?
            toText <$> createDBInstanceBackupRetentionPeriod
        , "CharacterSetName" |=?
            createDBInstanceCharacterSetName
        , "DBInstanceClass" |=
            toText createDBInstanceDBInstanceClass
        , "DBInstanceIdentifier" |=
            createDBInstanceDBInstanceIdentifier
        , "DBName" |=? createDBInstanceDBName
        , "DBParameterGroupName" |=?
            createDBInstanceDBParameterGroupName
        , "DBSecurityGroups.member" |.#=
            createDBInstanceDBSecurityGroups
        , "DBSubnetGroupName" |=?
            createDBInstanceDBSubnetGroupName
        , "Engine" |= toText createDBInstanceEngine
        , "EngineVersion" |=? createDBInstanceEngineVersion
        , "Iops" |=? toText <$> createDBInstanceIops
        , "LicenseModel" |=?
            toText <$> createDBInstanceLicenseModel
        , "MasterUserPassword" |= createDBInstanceMasterUserPassword
        , "MasterUsername" |= createDBInstanceMasterUsername
        , "MultiAZ" |=? boolToText <$> createDBInstanceMultiAZ
        , "OptionGroupName" |=? createDBInstanceOptionGroupName
        , "Port" |=? toText <$> createDBInstancePort
        , "PreferredBackupWindow" |=?
            createDBInstancePreferredBackupWindow
        , "PreferredMaintenanceWindow" |=?
            createDBInstancePreferredMaintenanceWindow
        , "PubliclyAccessible" |=?
            boolToText <$> createDBInstancePubliclyAccessible
        , "VpcSecurityGroupIds" |.#=
            createDBInstanceVpcSecurityGroupIds
        ]
