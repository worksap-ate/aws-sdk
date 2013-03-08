{-# LANGUAGE FlexibleContexts, RankNTypes, RecordWildCards #-}

module Cloud.AWS.RDS.DBInstance
    ( describeDBInstances
    , createDBInstance
    , deleteDBInstance
    , createDBInstanceReadReplica
    , promoteReadReplica
    , rebootDBInstance
    , restoreDBInstanceFromDBSnapshot
    , modifyDBInstance
    , describeOrderableDBInstanceOptions
    , restoreDBInstanceToPointInTime
    , describeReservedDBInstancesOfferings
    ) where

import Data.Text (Text)
import Data.Conduit
import Control.Applicative
import Data.XML.Types (Event(..))
import Data.Maybe (catMaybes)

import Cloud.AWS.Util
import Cloud.AWS.Lib.Query
import Cloud.AWS.Lib.Parser

import Cloud.AWS.RDS.Types hiding (Event)
import Cloud.AWS.RDS.Internal

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

promoteReadReplica
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Int -- ^ BackupRetentionPeriod
    -> Text -- ^ DBInstanceIdentifier
    -> Maybe Text -- ^ PreferredBackupWindow
    -> RDS m DBInstance
promoteReadReplica period dbiid window =
    rdsQuery "PromoteReadReplica" params $
        element "DBInstance" sinkDBInstance
  where
    params =
        [ "BackupRetentionPeriod" |=? toText <$> period
        , "DBInstanceIdentifier" |= dbiid
        , "PreferredBackupWindow" |=? window
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

modifyDBInstance
    :: (MonadBaseControl IO m, MonadResource m)
    => ModifyDBInstanceRequest
    -> RDS m DBInstance
modifyDBInstance ModifyDBInstanceRequest{..} =
    rdsQuery "ModifyDBInstance" params $
        element "DBInstance" sinkDBInstance
  where
    params =
        [ "AllocatedStorage" |=? toText <$>
            modifyDBInstanceRequestAllocatedStorage
        , "AllowMajorVersionUpgrade" |=? boolToText <$>
            modifyDBInstanceRequestAllowMajorVersionUpgrade
        , "ApplyImmediately" |=? boolToText <$>
            modifyDBInstanceRequestApplyImmediately
        , "AutoMinorVersionUpgrade" |=? boolToText <$>
            modifyDBInstanceRequestAutoMinorVersionUpgrade
        , "BackupRetentionPeriod" |=? toText <$>
            modifyDBInstanceRequestBackupRetentionPeriod
        , "DBInstanceClass" |=?
            modifyDBInstanceRequestDBInstanceClass
        , "DBInstanceIdentifier" |=
            modifyDBInstanceRequestDBInstanceIdentifier
        , "DBParameterGroupName" |=?
            modifyDBInstanceRequestDBParameterGroupName
        , "DBSecurityGroups.member" |.#=
            modifyDBInstanceRequestDBSecurityGroups
        , "EngineVersion" |=?
            modifyDBInstanceRequestEngineVersion
        , "Iops" |=? toText <$>
            modifyDBInstanceRequestIops
        , "MasterUserPassword" |=?
            modifyDBInstanceRequestMasterUserPassword
        , "MultiAZ" |=? boolToText <$>
            modifyDBInstanceRequestMultiAZ
        , "NewDBInstanceIdentifier" |=?
            modifyDBInstanceRequestNewDBInstanceIdentifier
        , "OptionGroupName" |=?
            modifyDBInstanceRequestOptionGroupName
        , "PreferredBackupWindow" |=?
            modifyDBInstanceRequestPrefferedBackupWindow
        , "PreferredMaintenanceWindow" |=?
            modifyDBInstanceRequestPrefferedMaintenanceWindow
        , "VpcSecurityGroups.member" |.#=
            modifyDBInstanceRequestVpcSecurityGroupIds
        ]

describeOrderableDBInstanceOptions
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe DBInstanceClass -- ^ DBInstanceClass
    -> Engine -- ^ Engine
    -> Maybe Text -- ^ EngineVersion
    -> Maybe LicenseModel -- ^ LicenseModel
    -> Maybe Bool -- ^ Vpc
    -> Maybe Text -- ^ Marker
    -> Maybe Int -- ^ MaxRecords
    -> RDS m (Maybe Text, [OrderableDBInstanceOption]) -- ^ (Marker, OrderableDBInstanceOptions)
describeOrderableDBInstanceOptions class' engine ver license vpc marker maxRec =
    rdsQuery "DescribeOrderableDBInstanceOptions" params $ (,)
        <$> getT "Marker"
        <*> elements "OrderableDBInstanceOption" orderableDBInstanceOptionSink
  where
    params =
        [ "DBInstanceClass" |=? class'
        , "Engine" |= engine
        , "EngineVersion" |=? ver
        , "LicenseModel" |=? toText <$> license
        , "Vpc" |=? boolToText <$> vpc
        , "Marker" |=? marker
        , "MaxRecords" |=? toText <$> maxRec
        ]

orderableDBInstanceOptionSink
    :: MonadThrow m
    => Consumer Event m OrderableDBInstanceOption
orderableDBInstanceOptionSink = OrderableDBInstanceOption
    <$> getT "MultiAZCapable"
    <*> getT "Engine"
    <*> getT "LicenseModel"
    <*> getT "ReadReplicaCapable"
    <*> getT "Vpc"
    <*> getT "EngineVersion"
    <*> elements "AvailabilityZone" (
        AvailabilityZone
        <$> getT "Name"
        <*> getT "ProvisionedIopsCapable"
        )
    <*> getT "DBInstanceClass"

restoreDBInstanceToPointInTime
    :: (MonadBaseControl IO m, MonadResource m)
    => RestoreTime
    -> RestoreDBInstanceToPointInTimeRequest
    -> RDS m DBInstance
restoreDBInstanceToPointInTime restore RestoreDBInstanceToPointInTimeRequest{..} =
    rdsQuery "RestoreDBInstanceToPointInTime" params $
        element "DBInstance" sinkDBInstance
  where
    params =
        [ "SourceDBInstanceIdentifier" |=
            restoreDBInstanceToPointInTimeSourceDBInstanceIdentifier
        , "TargetDBInstanceIdentifier" |=
            restoreDBInstanceToPointInTimeTargetDBInstanceIdentifier
        , "AutoMinorVersionUpgrade" |=? boolToText <$>
            restoreDBInstanceToPointInTimeAutoMinorVersionUpgrade
        , "AvailabilityZone" |=?
            restoreDBInstanceToPointInTimeAvailabilityZone
        , "DBInstanceClass" |=?
            restoreDBInstanceToPointInTimeDBInstanceClass
        , "DBName" |=?
            restoreDBInstanceToPointInTimeDBName
        , "DBSubnetGroupName" |=?
            restoreDBInstanceToPointInTimeDBSubnetGroupName
        , "Engine" |=?
            restoreDBInstanceToPointInTimeEngine
        , "Iops" |=? toText <$>
            restoreDBInstanceToPointInTimeIops
        , "LicenseModel" |=? toText <$>
            restoreDBInstanceToPointInTimeLicenseModel
        , "MultiAZ" |=? boolToText <$>
            restoreDBInstanceToPointInTimeMultiAZ
        , "OptionGroupName" |=?
            restoreDBInstanceToPointInTimeOptionGroupName
        , "Port" |=? toText <$>
            restoreDBInstanceToPointInTimePort
        , "PubliclyAccessible" |=? boolToText <$>
            restoreDBInstanceToPointInTimePubliclyAccessible
        , restoreTimeParam restore
        ]
    restoreTimeParam UseLatestRestorableTime =
        "UseLatestRestorableTime" |= boolToText True
    restoreTimeParam (RestoreTime time) =
        "RestoreTime" |= toText time

describeReservedDBInstancesOfferings
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Text -- ^ ReservedDBInstancesOfferingId
    -> Maybe DBInstanceClass -- ^ DBInstanceClass
    -> Maybe Int -- ^ Duration
    -> Maybe Bool -- ^ MultiAZ
    -> Maybe Text -- ^ OfferingType
    -> Maybe Text -- ^ ProductDescription
    -> Maybe Text -- ^ Marker
    -> Maybe Int -- ^ MaxRecords
    -> RDS m (Maybe Text, [ReservedDBInstancesOffering]) -- ^ (Marker, ReservedDBInstancesOfferings)
describeReservedDBInstancesOfferings oid class' dur az offer desc marker maxRec =
    rdsQuery "DescribeReservedDBInstancesOfferings" params $ (,)
        <$> getT "Marker"
        <*> elements "ReservedDBInstancesOffering" reservedDBInstancesOfferingSink
  where
    params =
        [ "ReservedDBInstancesOfferingId" |=? oid
        , "DBInstanceClass" |=? class'
        , "Duration" |=? toText <$> dur
        , "MultiAZ" |=? boolToText <$> az
        , "OfferingType" |=? offer
        , "ProductDescription" |=? desc
        , "Marker" |=? marker
        , "MaxRecords" |=? toText <$> maxRec
        ]

reservedDBInstancesOfferingSink
    :: MonadThrow m
    => Consumer Event m ReservedDBInstancesOffering
reservedDBInstancesOfferingSink = ReservedDBInstancesOffering
    <$> getT "OfferingType"
    <*> getT "Duration"
    <*> getT "CurrencyCode"
    <*> elements "RecurringCharge" (
        RecurringCharge
        <$> getT "RecurringChargeFrequency"
        <*> getT "RecurringChargeAmount"
        )
    <*> getT "FixedPrice"
    <*> getT "ProductDescription"
    <*> getT "UsagePrice"
    <*> getT "ReservedDBInstancesOfferingId"
    <*> getT "MultiAZ"
    <*> getT "DBInstanceClass"
