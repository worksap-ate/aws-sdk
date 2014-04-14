{-# LANGUAGE FlexibleContexts, RankNTypes, RecordWildCards, CPP #-}

module AWS.RDS.DBInstance
    ( describeDBInstances
    , createDBInstance
    , deleteDBInstance
    , createDBInstanceReadReplica
    , rebootDBInstance
    , restoreDBInstanceFromDBSnapshot
    ) where

import Data.Text (Text)
import Data.Conduit
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)
#endif
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
    => Consumer Event m DBInstance
sinkDBInstance = DBInstance
    <$> getT "Iops"
    <*> getT "BackupRetentionPeriod"
    <*> getT "DBInstanceStatus"
    <*> getT "MultiAZ"
    <*> elements' "VpcSecurityGroups" "VpcSecurityGroupMembership"
        vpcSecurityGroupMembershipSink
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
    <*> elements "DBSecurityGroup" dbSecurityGroupMembershipSink
    <*> getT "AutoMinorVersionUpgrade"
    <*> getT "DBName"
    <*> getT "InstanceCreateTime"
    <*> getT "AllocatedStorage"
    <*> getT "DBInstanceClass"
    <*> getT "MasterUsername"

sinkPendingModifiedValues
    :: MonadThrow m
    => Consumer Event m [PendingModifiedValue]
sinkPendingModifiedValues = element "PendingModifiedValues" $
    catMaybes <$> sequence
        [ f PendingModifiedValueMasterUserPassword "MasterUserPassword"
        , f PendingModifiedValueIops "Iops"
        , f PendingModifiedValueMultiAZ "MultiAZ"
        , f PendingModifiedValueAllocatedStorage "AllocatedStorage"
        , f PendingModifiedValueEngineVersion "EngineVersion"
        , f PendingModifiedValueDBInstanceIdentifier "DBInstanceIdentifier"
        , f PendingModifiedValueDBInstanceClass "DBInstanceClass"
        , f PendingModifiedValueBackupRetentionPeriod "BackupRetentionPeriod"
        , f PendingModifiedValuePort "Port"
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
        , "DBInstanceClass" |= createDBInstanceClass
        , "DBInstanceIdentifier" |=
            createDBInstanceIdentifier
        , "DBName" |=? createDBInstanceDBName
        , "DBParameterGroupName" |=?
            createDBInstanceDBParameterGroupName
        , "DBSecurityGroups.member" |.#=
            createDBInstanceDBSecurityGroups
        , "DBSubnetGroupName" |=?
            createDBInstanceDBSubnetGroupName
        , "Engine" |= createDBInstanceEngine
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

deleteDBInstance
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ DBInstanceIdentifier
    -> FinalSnapshot -- ^ FinalSnapshot
    -> RDS m DBInstance
deleteDBInstance dbiid final =
    rdsQuery "DeleteDBInstance" params $
        element "DBInstance" sinkDBInstance
  where
    params =
        [ "DBInstanceIdentifier" |= dbiid
        ] ++ finalSnapshotParams final
    finalSnapshotParams SkipFinalSnapshot =
        [ "SkipFinalSnapshot" |= boolToText True ]
    finalSnapshotParams (FinalSnapshotIdentifier sid) =
        [ "SkipFinalSnapshot" |= boolToText False
        , "FinalDBSnapshotIdentifier" |= sid
        ]

createDBInstanceReadReplica
    :: (MonadBaseControl IO m, MonadResource m)
    => CreateReadReplicaRequest
    -> RDS m DBInstance
createDBInstanceReadReplica CreateReadReplicaRequest{..} =
    rdsQuery "CreateDBInstanceReadReplica" params $
        element "DBInstance" sinkDBInstance
  where
    params =
        [ "AutoMinorVersionUpgrade" |=?
            boolToText <$> createReadReplicaAutoMinorVersionUpgrade
        , "AvailabilityZone" |=?
            createReadReplicaAvailabilityZone
        , "DBInstanceClass" |=
            createReadReplicaDBInstanceClass
        , "DBInstanceIdentifier" |=
            createReadReplicaDBInstanceIdentifier
        , "Iops" |=? toText <$> createReadReplicaIops
        , "OptionGroupName" |=? createReadReplicaOptionGroupName
        , "Port" |=? toText <$> createReadReplicaPort
        , "PubliclyAccessible" |=?
            boolToText <$> createReadReplicaPubliclyAccessible
        , "SourceDBInstanceIdentifier" |=
            createReadReplicaSourceDBInstanceIdentifier
        ]

rebootDBInstance
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ DBInstanceIdentifier
    -> Maybe Bool -- ^ ForceFailover
    -> RDS m DBInstance
rebootDBInstance dbiid force =
    rdsQuery "RebootDBInstance" params $
        element "DBInstance" sinkDBInstance
  where
    params =
        [ "DBInstanceIdentifier" |= dbiid
        , "ForceFailover" |=? boolToText <$> force
        ]

restoreDBInstanceFromDBSnapshot
    :: (MonadBaseControl IO m, MonadResource m)
    => RestoreDBInstanceFromDBSnapshotRequest
    -> RDS m DBInstance
restoreDBInstanceFromDBSnapshot RestoreDBInstanceFromDBSnapshotRequest{..} =
    rdsQuery "RestoreDBInstanceFromDBSnapshot" params $
        element "DBInstance" sinkDBInstance
  where
    params =
        [ "AutoMinorVersionUpgrade" |=? boolToText <$>
            restoreDBInstanceFromDBSnapshotRequestAutoMinorVersionUpgrade
        , "AvailabilityZone" |=?
            restoreDBInstanceFromDBSnapshotRequestAvailabilityZone
        , "DBInstanceClass" |=?
            restoreDBInstanceFromDBSnapshotRequestDBInstanceClass
        , "DBInstanceIdentifier" |=
            restoreDBInstanceFromDBSnapshotRequestDBInstanceIdentifier
        , "DBName" |=? restoreDBInstanceFromDBSnapshotRequestDBName
        , "DBSnapshotIdentifier" |=
            restoreDBInstanceFromDBSnapshotRequestDBSnapshotIdentifier
        , "DBSubnetGroupName" |=?
            restoreDBInstanceFromDBSnapshotRequestDBSubnetGroupName
        , "Engine" |=? restoreDBInstanceFromDBSnapshotRequestEngine
        , "Iops" |=? toText <$>
            restoreDBInstanceFromDBSnapshotRequestIops
        , "LicenseModel" |=? toText <$>
            restoreDBInstanceFromDBSnapshotRequestLicenseModel
        , "MultiAZ" |=? boolToText <$>
            restoreDBInstanceFromDBSnapshotRequestMultiAZ
        , "OptionGroupName" |=?
            restoreDBInstanceFromDBSnapshotRequestOptionGroupName
        , "Port" |=? toText <$>
            restoreDBInstanceFromDBSnapshotRequestPort
        , "PubliclyAccessible" |=? boolToText <$>
            restoreDBInstanceFromDBSnapshotRequestPubliclyAccessible
        ]
