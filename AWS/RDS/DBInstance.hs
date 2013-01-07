{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.RDS.DBInstance
    ( describeDBInstances
    ) where

import Data.Text (Text)
import Data.Conduit
import Control.Applicative
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.XML.Types (Event(..))
import Data.Maybe (catMaybes)

import AWS.Util
import AWS.Lib.Query
import AWS.Lib.Parser

import AWS.RDS.Types
import AWS.RDS.Internal

describeDBInstances
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Text -- ^ DBInstanceIdentifier
    -> Maybe Int -- ^ MaxRecords
    -> Maybe Text -- ^ Marker
    -> RDS m [DBInstance]
describeDBInstances dbid maxRecords marker =
    rdsQuery "DescribeDBInstances" params sinkDBInstances
  where
    params =
        [ "DBInstanceIdentifier" |=? dbid
        , "MaxRecords" |=? toText <$> maxRecords
        , "Marker" |=? marker
        ]

sinkDBInstances
    :: MonadThrow m
    => GLSink Event m [DBInstance]
sinkDBInstances = elements "DBInstance" $
    DBInstance
    <$> getT "Iops"
    <*> getT "BackupRetentionPeriod"
    <*> getT "MultiAZ"
    <*> getT "DBInstanceStatus"
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
    <*> elementM "DBSubnetGroup"
        (DBSubnetGroup
        <$> getT "VpcId"
        <*> getT "SubnetGroupStatus"
        <*> getT "DBSubnetGroupDescription"
        <*> getT "DBSubnetGroupName"
        <*> elements "Subnet"
            (Subnet
            <$> getT "SubnetStatus"
            <*> getT "SubnetIdentifier"
            <*> element "SubnetAvailabilityZone"
                (AvailabilityZone
                <$> getT "Name"
                <*> getT "ProvisionedIopsCapable"
                )
            )
        )
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
    <*> elements "DBSecurityGroup"
        (DBSecurityGroupMembership
        <$> getT "Status"
        <*> getT "DBSecurityGroupName"
        )
    <*> getT "DBName"
    <*> getT "AutoMinorVersionUpgrade"
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
