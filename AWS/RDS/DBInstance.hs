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
    params = maybeParams
        [ ("DBInstanceIdentifier", dbid)
        , ("MaxRecords", toText <$> maxRecords)
        , ("Marker", marker)
        ]

sinkDBInstances
    :: MonadThrow m
    => GLSink Event m [DBInstance]
sinkDBInstances = elements "DBInstance" $
    DBInstance
    <$> getMT "Iops"
    <*> getT "BackupRetentionPeriod"
    <*> getF "MultiAZ" textToBool
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
    <*> getM "LatestRestorableTime" (textToTime <$>)
    <*> elements "ReadReplicaDBInstanceIdentifier" text
    <*> getT "Engine"
    <*> sinkPendingModifiedValues
    <*> getMT "CharacterSetName"
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
                <*> getF "ProvisionedIopsCapable" textToBool
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
    <*> getMT "ReadReplicaSourceDBInstanceIdentifier"
    <*> elements "DBSecurityGroup"
        (DBSecurityGroupMembership
        <$> getT "Status"
        <*> getT "DBSecurityGroupName"
        )
    <*> getMT "DBName"
    <*> getF "AutoMinorVersionUpgrade" textToBool
    <*> getM "InstanceCreateTime" (textToTime <$>)
    <*> getT "AllocatedStorage"
    <*> getT "DBInstanceClass"
    <*> getT "MasterUsername" 

sinkPendingModifiedValues
    :: MonadThrow m
    => GLSink Event m [PendingModifiedValue]
sinkPendingModifiedValues = element "PendingModifiedValues" $
    catMaybes <$> sequence [ m "MasterUserPassword" PMVMasterUserPassword
        , m "Iops" (PMVIops . textRead)
        , m "MultiAZ" (PMVMultiAZ . textToBool)
        , m "AllocatedStorage" (PMVAllocatedStorage . textRead)
        , m "EngineVersion" PMVEngineVersion
        , m "DBInstanceClass" PMVDBInstanceClass
        , m "BackupRetentionPeriod"
            (PMVBackupRetentionPeriod . textRead)
        , m "Port" (PMVPort . textRead)
        ]
  where
    m t f = getM t (f <$>)
