{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Instance
    ( describeInstances
    ) where

import           Data.ByteString (ByteString)

import Data.XML.Types (Event)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative

import AWS.EC2.Types
import AWS.EC2.Query
import AWS.EC2.Parser

describeInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => [ByteString]
    -> [Filter]
    -> EC2 m (EC2Response (Source m Reservation))
describeInstances instances filters =
    ec2Query "DescribeInstances" params reservationSet
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
    <*> element "instanceState" (
        codeToState
        <$> getF "code" t2dec
        <* getT "name"
        )
    <*> getT "privateDnsName"
    <*> getT "dnsName"
    <*> getT "reason"
    <*> getT "keyName"
    <*> getT "amiLaunchIndex"
    <*> productCodeSink
    <*> getT "instanceType"
    <*> getF "launchTime" t2time
    <*> element "placement" (
        placement
        <$> getT "availabilityZone"
        <*> getT "groupName"
        <*> getT "tenancy"
        )
    <*> getMT "kernelId"
    <*> getMT "ramdiskId"
    <*> getMT "platform"
    <*> element "monitoring" (getF "state" t2monitoring)
    <*> getMT "subnetId"
    <*> getMT "vpcId"
    <*> getMT "privateIpAddress"
    <*> getMT "ipAddress"
    <*> getM "sourceDestCheck" (t2bool <$>)
    <*> groupSetSink
    <*> stateReasonSink
    <*> getF "architecture" t2architecture
    <*> getF "rootDeviceType" t2deviceType
    <*> getMT "rootDeviceName"
    <*> itemsSet "blockDeviceMapping" (
        instanceBlockDeviceMapping
        <$> getT "deviceName"
        <*> element "ebs" (
            instanceEbsBlockDevice
            <$> getT "volumeId"
            <*> getF "status" t2volumeState
            <*> getF "attachTime" t2time
            <*> getF "deleteOnTermination" t2bool
            )
        )
    <*> getM "instanceLifecycle" t2lifecycle
    <*> getMT "spotInstanceRequestId"
    <*> getF "virtualizationType" t2virtualizationType
    <*> getT "clientToken"
    <*> resourceTagSink
    <*> getF "hypervisor" t2hypervisor
    <*> networkInterfaceSink
    <*> elementM "iamInstanceProfile" (
        iamInstanceProfile
        <$> getT "arn"
        <*> getT "id"
        )
    <*> getF "ebsOptimized" t2bool

networkInterfaceSink :: MonadThrow m
    => GLSink Event m [InstanceNetworkInterface]
networkInterfaceSink = itemsSet "networkInterfaceSet" $
    instanceNetworkInterface
    <$> getT "networkInterfaceId"
    <*> getT "subnetId"
    <*> getT "vpcId"
    <*> getM "description" t2emptxt
    <*> getT "ownerId"
    <*> getT "status"
    <*> getT "privateIpAddress"
    <*> getMT "privateDnsName"
    <*> getF "sourceDestCheck" t2bool
    <*> groupSetSink
    <*> element "attachment" (
        networkInterfaceAttachment
        <$> getT "attachmentId"
        <*> getF "deviceIndex" t2dec
        <*> getT "status"
        <*> getF "attachTime" t2time
        <*> getF "deleteOnTermination" t2bool
        )
    <*> niAssociationSink
    <*> itemsSet "privateIpAddressesSet" (
        instancePrivateIpAddress
        <$> getT "privateIpAddress"
        <*> getF "primary" t2bool
        <*> niAssociationSink
        )

niAssociationSink :: MonadThrow m
    => GLSink Event m (Maybe NetworkInterfaceAssociation)
niAssociationSink = elementM "association" $
    networkInterfaceAssociation
    <$> getT "publicIp"
    <*> getT "ipOwnerId"
