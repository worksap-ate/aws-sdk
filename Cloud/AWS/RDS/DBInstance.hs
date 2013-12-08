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
import Data.Maybe (catMaybes, fromMaybe)

import Cloud.AWS.Lib.Query
import Cloud.AWS.Lib.Parser.Unordered (SimpleXML, (.<), getElement, getElementM, content)

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
        , "MaxRecords" |=? maxRecords
        , "Marker" |=? marker
        ]

sinkDBInstance
    :: (MonadThrow m, Applicative m)
    => SimpleXML -> m DBInstance
sinkDBInstance xml = DBInstance
    <$> xml .< "Iops"
    <*> xml .< "BackupRetentionPeriod"
    <*> xml .< "DBInstanceStatus"
    <*> xml .< "MultiAZ"
    <*> elements' "VpcSecurityGroups" "VpcSecurityGroupMembership"
        vpcSecurityGroupMembershipSink xml
    <*> xml .< "DBInstanceIdentifier"
    <*> xml .< "PreferredBackupWindow"
    <*> xml .< "PreferredMaintenanceWindow"
    <*> xml .< "AvailabilityZone"
    <*> xml .< "LatestRestorableTime"
    <*> elements "ReadReplicaDBInstanceIdentifier" content xml
    <*> xml .< "Engine"
    <*> sinkPendingModifiedValues xml
    <*> xml .< "CharacterSetName"
    <*> xml .< "LicenseModel"
    <*> getElementM xml "DBSubnetGroup" dbSubnetGroupSink
    <*> elements "DBParameterGroup"
        (\xml' -> DBParameterGroupStatus
        <$> xml' .< "ParameterApplyStatus"
        <*> xml' .< "DBParameterGroupName"
        ) xml
    <*> getElementM xml "Endpoint"
        (\xml' -> Endpoint
        <$> xml' .< "Port"
        <*> xml' .< "Address"
        )
    <*> xml .< "EngineVersion"
    <*> xml .< "ReadReplicaSourceDBInstanceIdentifier"
    <*> elements "OptionGroupMembership"
        (\xml' -> OptionGroupMembership
        <$> xml' .< "OptionGroupName"
        <*> xml' .< "Status"
        ) xml
    <*> xml .< "PubliclyAccessible"
    <*> elements "DBSecurityGroup" dbSecurityGroupMembershipSink xml
    <*> xml .< "AutoMinorVersionUpgrade"
    <*> xml .< "DBName"
    <*> xml .< "InstanceCreateTime"
    <*> xml .< "AllocatedStorage"
    <*> xml .< "DBInstanceClass"
    <*> xml .< "MasterUsername"

sinkPendingModifiedValues
    :: (MonadThrow m, Applicative m)
    => SimpleXML -> m [PendingModifiedValue]
sinkPendingModifiedValues sxml = fromMaybe [] <$> getElementM sxml "PendingModifiedValue" (\xml ->
    catMaybes <$> sequence
        [ f xml PendingModifiedValueMasterUserPassword "MasterUserPassword"
        , f xml PendingModifiedValueIops "Iops"
        , f xml PendingModifiedValueMultiAZ "MultiAZ"
        , f xml PendingModifiedValueAllocatedStorage "AllocatedStorage"
        , f xml PendingModifiedValueEngineVersion "EngineVersion"
        , f xml PendingModifiedValueDBInstanceIdentifier "DBInstanceIdentifier"
        , f xml PendingModifiedValueDBInstanceClass "DBInstanceClass"
        , f xml PendingModifiedValueBackupRetentionPeriod "BackupRetentionPeriod"
        , f xml PendingModifiedValuePort "Port"
        ]
    )
  where
    f xml c name = fmap c <$> xml .< name

createDBInstance
    :: (MonadBaseControl IO m, MonadResource m)
    => CreateDBInstanceRequest -- ^ data type of CreateDBInstance
    -> RDS m DBInstance
createDBInstance CreateDBInstanceRequest{..} =
    rdsQuery "CreateDBInstance" params $ \xml ->
        getElement xml "DBInstance" sinkDBInstance
  where
    params =
        [ "AllocatedStorage" |=
            createDBInstanceAllocatedStorage
        , "AutoMinorVersionUpgrade" |=?
            createDBInstanceAutoMinorVersionUpgrade
        , "AvailabilityZone" |=?
            createDBInstanceAvailabilityZone
        , "BackupRetentionPeriod" |=?
            createDBInstanceBackupRetentionPeriod
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
        , "Iops" |=? createDBInstanceIops
        , "LicenseModel" |=?
            createDBInstanceLicenseModel
        , "MasterUserPassword" |= createDBInstanceMasterUserPassword
        , "MasterUsername" |= createDBInstanceMasterUsername
        , "MultiAZ" |=? createDBInstanceMultiAZ
        , "OptionGroupName" |=? createDBInstanceOptionGroupName
        , "Port" |=? createDBInstancePort
        , "PreferredBackupWindow" |=?
            createDBInstancePreferredBackupWindow
        , "PreferredMaintenanceWindow" |=?
            createDBInstancePreferredMaintenanceWindow
        , "PubliclyAccessible" |=?
            createDBInstancePubliclyAccessible
        , "VpcSecurityGroupIds" |.#=
            createDBInstanceVpcSecurityGroupIds
        ]

deleteDBInstance
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ DBInstanceIdentifier
    -> FinalSnapshot -- ^ FinalSnapshot
    -> RDS m DBInstance
deleteDBInstance dbiid final =
    rdsQuery "DeleteDBInstance" params $ \xml ->
        getElement xml "DBInstance" sinkDBInstance
  where
    params =
        [ "DBInstanceIdentifier" |= dbiid
        ] ++ finalSnapshotParams final
    finalSnapshotParams SkipFinalSnapshot =
        [ "SkipFinalSnapshot" |= True ]
    finalSnapshotParams (FinalSnapshotIdentifier sid) =
        [ "SkipFinalSnapshot" |= False
        , "FinalDBSnapshotIdentifier" |= sid
        ]

createDBInstanceReadReplica
    :: (MonadBaseControl IO m, MonadResource m)
    => CreateReadReplicaRequest
    -> RDS m DBInstance
createDBInstanceReadReplica CreateReadReplicaRequest{..} =
    rdsQuery "CreateDBInstanceReadReplica" params $ \xml ->
        getElement xml "DBInstance" sinkDBInstance
  where
    params =
        [ "AutoMinorVersionUpgrade" |=?
            createReadReplicaAutoMinorVersionUpgrade
        , "AvailabilityZone" |=?
            createReadReplicaAvailabilityZone
        , "DBInstanceClass" |=
            createReadReplicaDBInstanceClass
        , "DBInstanceIdentifier" |=
            createReadReplicaDBInstanceIdentifier
        , "Iops" |=? createReadReplicaIops
        , "OptionGroupName" |=? createReadReplicaOptionGroupName
        , "Port" |=? createReadReplicaPort
        , "PubliclyAccessible" |=?
            createReadReplicaPubliclyAccessible
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
    rdsQuery "PromoteReadReplica" params $ \xml ->
        getElement xml "DBInstance" sinkDBInstance
  where
    params =
        [ "BackupRetentionPeriod" |=? period
        , "DBInstanceIdentifier" |= dbiid
        , "PreferredBackupWindow" |=? window
        ]

rebootDBInstance
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ DBInstanceIdentifier
    -> Maybe Bool -- ^ ForceFailover
    -> RDS m DBInstance
rebootDBInstance dbiid force =
    rdsQuery "RebootDBInstance" params $ \xml ->
        getElement xml "DBInstance" sinkDBInstance
  where
    params =
        [ "DBInstanceIdentifier" |= dbiid
        , "ForceFailover" |=? force
        ]

restoreDBInstanceFromDBSnapshot
    :: (MonadBaseControl IO m, MonadResource m)
    => RestoreDBInstanceFromDBSnapshotRequest
    -> RDS m DBInstance
restoreDBInstanceFromDBSnapshot RestoreDBInstanceFromDBSnapshotRequest{..} =
    rdsQuery "RestoreDBInstanceFromDBSnapshot" params $ \xml ->
        getElement xml "DBInstance" sinkDBInstance
  where
    params =
        [ "AutoMinorVersionUpgrade" |=?
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
        , "Iops" |=? restoreDBInstanceFromDBSnapshotRequestIops
        , "LicenseModel" |=?
            restoreDBInstanceFromDBSnapshotRequestLicenseModel
        , "MultiAZ" |=?
            restoreDBInstanceFromDBSnapshotRequestMultiAZ
        , "OptionGroupName" |=?
            restoreDBInstanceFromDBSnapshotRequestOptionGroupName
        , "Port" |=?  restoreDBInstanceFromDBSnapshotRequestPort
        , "PubliclyAccessible" |=?
            restoreDBInstanceFromDBSnapshotRequestPubliclyAccessible
        ]

modifyDBInstance
    :: (MonadBaseControl IO m, MonadResource m)
    => ModifyDBInstanceRequest
    -> RDS m DBInstance
modifyDBInstance ModifyDBInstanceRequest{..} =
    rdsQuery "ModifyDBInstance" params $ \xml ->
        getElement xml "DBInstance" sinkDBInstance
  where
    params =
        [ "AllocatedStorage" |=?
            modifyDBInstanceRequestAllocatedStorage
        , "AllowMajorVersionUpgrade" |=?
            modifyDBInstanceRequestAllowMajorVersionUpgrade
        , "ApplyImmediately" |=?
            modifyDBInstanceRequestApplyImmediately
        , "AutoMinorVersionUpgrade" |=?
            modifyDBInstanceRequestAutoMinorVersionUpgrade
        , "BackupRetentionPeriod" |=?
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
        , "Iops" |=? modifyDBInstanceRequestIops
        , "MasterUserPassword" |=?
            modifyDBInstanceRequestMasterUserPassword
        , "MultiAZ" |=? modifyDBInstanceRequestMultiAZ
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
    rdsQuery "DescribeOrderableDBInstanceOptions" params $ \xml -> (,)
        <$> xml .< "Marker"
        <*> elements "OrderableDBInstanceOption" orderableDBInstanceOptionSink xml
  where
    params =
        [ "DBInstanceClass" |=? class'
        , "Engine" |= engine
        , "EngineVersion" |=? ver
        , "LicenseModel" |=? license
        , "Vpc" |=? vpc
        , "Marker" |=? marker
        , "MaxRecords" |=? maxRec
        ]

orderableDBInstanceOptionSink
    :: (MonadThrow m, Applicative m)
    => SimpleXML -> m OrderableDBInstanceOption
orderableDBInstanceOptionSink xml = OrderableDBInstanceOption
    <$> xml .< "MultiAZCapable"
    <*> xml .< "Engine"
    <*> xml .< "LicenseModel"
    <*> xml .< "ReadReplicaCapable"
    <*> xml .< "Vpc"
    <*> xml .< "EngineVersion"
    <*> elements "AvailabilityZone" (\xml' ->
        AvailabilityZone
        <$> xml' .< "Name"
        <*> xml' .< "ProvisionedIopsCapable"
        ) xml
    <*> xml .< "DBInstanceClass"

restoreDBInstanceToPointInTime
    :: (MonadBaseControl IO m, MonadResource m)
    => RestoreTime
    -> RestoreDBInstanceToPointInTimeRequest
    -> RDS m DBInstance
restoreDBInstanceToPointInTime restore RestoreDBInstanceToPointInTimeRequest{..} =
    rdsQuery "RestoreDBInstanceToPointInTime" params $ \xml ->
        getElement xml "DBInstance" sinkDBInstance
  where
    params =
        [ "SourceDBInstanceIdentifier" |=
            restoreDBInstanceToPointInTimeSourceDBInstanceIdentifier
        , "TargetDBInstanceIdentifier" |=
            restoreDBInstanceToPointInTimeTargetDBInstanceIdentifier
        , "AutoMinorVersionUpgrade" |=?
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
        , "Iops" |=? restoreDBInstanceToPointInTimeIops
        , "LicenseModel" |=?
            restoreDBInstanceToPointInTimeLicenseModel
        , "MultiAZ" |=? restoreDBInstanceToPointInTimeMultiAZ
        , "OptionGroupName" |=?
            restoreDBInstanceToPointInTimeOptionGroupName
        , "Port" |=? restoreDBInstanceToPointInTimePort
        , "PubliclyAccessible" |=?
            restoreDBInstanceToPointInTimePubliclyAccessible
        , restoreTimeParam restore
        ]
    restoreTimeParam UseLatestRestorableTime =
        "UseLatestRestorableTime" |= True
    restoreTimeParam (RestoreTime time) =
        "RestoreTime" |= time

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
    rdsQuery "DescribeReservedDBInstancesOfferings" params $ \xml -> (,)
        <$> xml .< "Marker"
        <*> elements "ReservedDBInstancesOffering" reservedDBInstancesOfferingSink xml
  where
    params =
        [ "ReservedDBInstancesOfferingId" |=? oid
        , "DBInstanceClass" |=? class'
        , "Duration" |=? dur
        , "MultiAZ" |=? az
        , "OfferingType" |=? offer
        , "ProductDescription" |=? desc
        , "Marker" |=? marker
        , "MaxRecords" |=? maxRec
        ]

reservedDBInstancesOfferingSink
    :: (MonadThrow m, Applicative m)
    => SimpleXML -> m ReservedDBInstancesOffering
reservedDBInstancesOfferingSink xml = ReservedDBInstancesOffering
    <$> xml .< "OfferingType"
    <*> xml .< "Duration"
    <*> xml .< "CurrencyCode"
    <*> elements "RecurringCharge" (\xml' ->
        RecurringCharge
        <$> xml' .< "RecurringChargeFrequency"
        <*> xml' .< "RecurringChargeAmount"
        ) xml
    <*> xml .< "FixedPrice"
    <*> xml .< "ProductDescription"
    <*> xml .< "UsagePrice"
    <*> xml .< "ReservedDBInstancesOfferingId"
    <*> xml .< "MultiAZ"
    <*> xml .< "DBInstanceClass"
