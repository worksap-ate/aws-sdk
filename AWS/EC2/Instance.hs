{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Instance
    ( describeInstances
    , startInstances
    , stopInstances
    , describeInstanceStatus
    ) where

import Data.Text (Text)

import Data.XML.Types (Event)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative

import AWS.EC2.Types
import AWS.EC2.Class
import AWS.EC2.Query
import AWS.EC2.Parser
import AWS.Util

------------------------------------------------------------
-- DescribeInstances
------------------------------------------------------------
describeInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ InstanceIds
    -> [Filter] -- ^ Filters
    -> EC2 m (Source m Reservation)
describeInstances instances filters = do
    ec2QuerySource "DescribeInstances" params reservationSet
  where
    params =
        [ ArrayParams "InstanceId" instances
        , FilterParams filters
        ]

reservationSet :: MonadThrow m
    => GLConduit Event m Reservation
reservationSet = itemConduit "reservationSet" $
    reservation
    <$> getT "reservationId"
    <*> getT "ownerId"
    <*> groupSetSink
    <*> instanceSetSink
    <*> getMT "requesterId"

groupSetSink :: MonadThrow m => GLSink Event m [Group]
groupSetSink = itemsSet "groupSet" $ group
    <$> getT "groupId"
    <*> getT "groupName"

instanceSetSink :: MonadThrow m
    => GLSink Event m [Instance]
instanceSetSink = itemsSet "instancesSet" $
    ec2Instance
    <$> getT "instanceId"
    <*> getT "imageId"
    <*> instanceStateSink "instanceState"
    <*> getT "privateDnsName"
    <*> getT "dnsName"
    <*> getT "reason"
    <*> getT "keyName"
    <*> getT "amiLaunchIndex"
    <*> productCodeSink
    <*> getT "instanceType"
    <*> getF "launchTime" textToTime
    <*> element "placement" (
        placement
        <$> getT "availabilityZone"
        <*> getT "groupName"
        <*> getT "tenancy"
        )
    <*> getMT "kernelId"
    <*> getMT "ramdiskId"
    <*> getMT "platform"
    <*> element "monitoring" (getF "state" instanceMonitoringState)
    <*> getMT "subnetId"
    <*> getMT "vpcId"
    <*> getMT "privateIpAddress"
    <*> getMT "ipAddress"
    <*> getM "sourceDestCheck" (textToBool <$>)
    <*> groupSetSink
    <*> stateReasonSink
    <*> getF "architecture" architecture
    <*> getF "rootDeviceType" rootDeviceType
    <*> getMT "rootDeviceName"
    <*> itemsSet "blockDeviceMapping" (
        instanceBlockDeviceMapping
        <$> getT "deviceName"
        <*> element "ebs" (
            instanceEbsBlockDevice
            <$> getT "volumeId"
            <*> getF "status" volumeState
            <*> getF "attachTime" textToTime
            <*> getF "deleteOnTermination" textToBool
            )
        )
    <*> getM "instanceLifecycle" instanceLifecycle
    <*> getMT "spotInstanceRequestId"
    <*> getF "virtualizationType" virtualizationType
    <*> getT "clientToken"
    <*> resourceTagSink
    <*> getF "hypervisor" hypervisor
    <*> networkInterfaceSink
    <*> elementM "iamInstanceProfile" (
        iamInstanceProfile
        <$> getT "arn"
        <*> getT "id"
        )
    <*> getF "ebsOptimized" textToBool

instanceStateSink :: MonadThrow m
    => Text -> GLSink Event m InstanceState
instanceStateSink label = element label $
    codeToState
    <$> getF "code" textToInt
    <* getT "name"

networkInterfaceSink :: MonadThrow m
    => GLSink Event m [InstanceNetworkInterface]
networkInterfaceSink = itemsSet "networkInterfaceSet" $
    instanceNetworkInterface
    <$> getT "networkInterfaceId"
    <*> getT "subnetId"
    <*> getT "vpcId"
    <*> getM "description" orEmpty
    <*> getT "ownerId"
    <*> getT "status"
    <*> getT "privateIpAddress"
    <*> getMT "privateDnsName"
    <*> getF "sourceDestCheck" textToBool
    <*> groupSetSink
    <*> element "attachment" (
        networkInterfaceAttachment
        <$> getT "attachmentId"
        <*> getF "deviceIndex" textToInt
        <*> getT "status"
        <*> getF "attachTime" textToTime
        <*> getF "deleteOnTermination" textToBool
        )
    <*> niAssociationSink
    <*> itemsSet "privateIpAddressesSet" (
        instancePrivateIpAddress
        <$> getT "privateIpAddress"
        <*> getF "primary" textToBool
        <*> niAssociationSink
        )

niAssociationSink :: MonadThrow m
    => GLSink Event m (Maybe NetworkInterfaceAssociation)
niAssociationSink = elementM "association" $
    networkInterfaceAssociation
    <$> getT "publicIp"
    <*> getT "ipOwnerId"

------------------------------------------------------------
-- DescribeInstanceStatus
------------------------------------------------------------
-- | raise 'ResponseParserException'('NextToken' token)
describeInstanceStatus
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ InstanceIds
    -> Bool  -- ^ is all instance? 'False': running instance only.
    -> [Filter] -- ^ Filters
    -> Maybe Text -- ^ next token
    -> EC2 m (Source m InstanceStatus)
describeInstanceStatus instanceIds isAll filters token =
    ec2QuerySource' "DescribeInstanceStatus" params token instanceStatusSet
  where
    params =
        [ ArrayParams "InstanceId" instanceIds
        , ValueParam "IncludeAllInstances" $ boolToText isAll
        , FilterParams filters
        ]

instanceStatusSet :: MonadThrow m
    => GLConduit Event m InstanceStatus
instanceStatusSet = do
    itemConduit "instanceStatusSet" $
        instanceStatus
        <$> getT "instanceId"
        <*> getT "availabilityZone"
        <*> itemsSet "eventsSet" (
            instanceStatusEvent
            <$> getF "code" instanceStatusEventCode
            <*> getT "description"
            <*> getM "notBefore" (textToTime <$>)
            <*> getM "notAfter" (textToTime <$>)
            )
        <*> instanceStateSink "instanceState"
        <*> instanceStatusTypeSink "systemStatus"
        <*> instanceStatusTypeSink "instanceStatus"

instanceStatusTypeSink :: MonadThrow m
    => Text -> GLSink Event m InstanceStatusType
instanceStatusTypeSink name = element name $
    instanceStatusType
    <$> getF "status" instanceStatusTypeStatus
    <*> itemsSet "details" (
        instanceStatusDetail
        <$> getT "name"
        <*> getT "status"
        <*> getM "impairedSince" (textToTime <$>)
        )

------------------------------------------------------------
-- StartInstances
------------------------------------------------------------
startInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ InstanceIds
    -> EC2 m (Source m InstanceStateChange)
startInstances instanceIds =
    ec2QuerySource "StartInstances" params instanceStateChangeSet
  where
    params = [ArrayParams "InstanceId" instanceIds]

instanceStateChangeSet
    :: (MonadResource m, MonadBaseControl IO m)
    => Conduit Event m InstanceStateChange
instanceStateChangeSet = itemConduit "instancesSet" $ do
    instanceStateChange
    <$> getT "instanceId"
    <*> instanceStateSink "currentState"
    <*> instanceStateSink "previousState"

------------------------------------------------------------
-- StopInstances
------------------------------------------------------------
stopInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ InstanceIds
    -> Bool -- ^ Force
    -> EC2 m (Source m InstanceStateChange)
stopInstances instanceIds force =
    ec2QuerySource "StopInstances" params instanceStateChangeSet
  where
    params =
        [ ArrayParams "InstanceId" instanceIds
        , ValueParam "Force" $ boolToText force]
