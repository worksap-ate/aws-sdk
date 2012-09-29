{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.EC2.Instance
    ( describeInstances
    , runInstances
    , RunInstancesParam(..)
    , defaultRunInstancesParam
    , terminateInstances
    , startInstances
    , stopInstances
    , rebootInstances
    , getConsoleOutput
    , getPasswordData
    , describeInstanceStatus
    ) where

import Data.Text (Text)
import Data.ByteString (ByteString)

import Data.XML.Types (Event)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative

import AWS.EC2.Types
import AWS.EC2.Params
import AWS.EC2.Utils
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
    ec2QuerySource "DescribeInstances" params $
        itemConduit "reservationSet" reservationSink
  where
    params =
        [ ArrayParams "InstanceId" instances
        , FilterParams filters
        ]

reservationSink :: MonadThrow m
    => GLSink Event m Reservation
reservationSink =
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
    <*> getMT "keyName"
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
            <*> getF "status" attachmentStatus
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

------------------------------------------------------------
-- RebootInstances
------------------------------------------------------------
rebootInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ InstanceIds
    -> EC2 m Bool
rebootInstances instanceIds =
    ec2Query "RebootInstances" params $ getF "return" textToBool
  where
    params = [ArrayParams "InstanceId" instanceIds]

------------------------------------------------------------
-- TerminateInstances
------------------------------------------------------------
terminateInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ InstanceIds
    -> EC2 m (Source m InstanceStateChange)
terminateInstances instanceIds =
    ec2QuerySource "TerminateInstances" params
        instanceStateChangeSet
  where
    params = [ArrayParams "InstanceId" instanceIds]

------------------------------------------------------------
-- RunInstances
------------------------------------------------------------
-- | 'RunInstancesParam' is genereted with 'defaultRunInstancesParam'
runInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => RunInstancesParam
    -> EC2 m Reservation
runInstances param =
    ec2Query "RunInstances" params reservationSink
  where
    mk name = maybe [] (\a -> [ValueParam name a])
    params =
        [ ValueParam "ImageId" $ riImageId param
        , ValueParam "MinCount" $ toText $ riMinCount param
        , ValueParam "MaxCount" $ toText $ riMaxCount param
        , ArrayParams "SecurityGroupId" $ riSecurityGroupIds param
        , ArrayParams "SecurityGroup" $ riSecurityGroups param
        , blockDeviceMappingParams $ riBlockDeviceMappings param
        ] ++ (uncurry mk =<<
            [ ("KeyName", riKeyName param) , ("UserData", bsToText <$> riUserData param)
            , ("InstanceType", riInstanceType param)
            , ("Placement.AvailabilityZone",
               riAvailabilityZone param)
            , ("Placement.GroupName", riPlacementGroup param)
            , ("Placement.Tenancy", riTenancy param)
            , ("KernelId", riKernelId param)
            , ("RamdiskId", riRamdiskId param)
            , ("Monitoring.Enabled",
               boolToText <$> riMonitoringEnabled param)
            , ("SubnetId", riSubnetId param)
            , ("DisableApiTermination",
               boolToText <$> riDisableApiTermination param)
            , ("InstanceInitiatedShutdownBehavior",
               sbToText <$> riShutdownBehavior param)
            , ("ClientToken", riClientToken param)
            , ("IamInstanceProfile.Arn",
               iipArn <$> riIamInstanceProfile param)
            , ("IamInstanceProfile.Name",
               iipId <$> riIamInstanceProfile param)
            , ("EbsOptimized", boolToText <$> riEbsOptimized param)
            ])

data RunInstancesParam = RunInstancesParam
    { riImageId :: Text -- ^ Required
    , riMinCount :: Int -- ^ Required
    , riMaxCount :: Int -- ^ Required
    , riKeyName :: Maybe Text
    , riSecurityGroupIds :: [Text]
      -- ^ SecurityGroupIds (Required for VPC; optional for EC2)
    , riSecurityGroups :: [Text]
      -- ^ SecurityGroups (Only for EC2; either id or name is accepted)
    , riUserData :: Maybe ByteString
      -- ^ UserData (Base64-encoded MIME user data)
    , riInstanceType :: Maybe Text
    , riAvailabilityZone :: Maybe Text
    , riPlacementGroup :: Maybe Text
    , riTenancy :: Maybe Text
    , riKernelId :: Maybe Text
    , riRamdiskId :: Maybe Text
    , riBlockDeviceMappings :: [BlockDeviceMappingParam]
    , riMonitoringEnabled :: Maybe Bool
    , riSubnetId :: Maybe Text
    , riDisableApiTermination :: Maybe Bool
    , riShutdownBehavior :: Maybe ShutdownBehavior
    , riPrivateIpAddresses :: [Text] -- ^ XXX: not implemented
    , riClientToken :: Maybe Text
    , riNetworkInterface :: [NetworkInterfaceParam] -- ^ XXX: not implemented
    , riIamInstanceProfile :: Maybe IamInstanceProfile
    , riEbsOptimized :: Maybe Bool
    }
  deriving (Show)

-- | RunInstances parameter utility
defaultRunInstancesParam
    :: Text -- ^ ImageId
    -> Int -- ^ MinCount
    -> Int -- ^ MaxCount
    -> RunInstancesParam
defaultRunInstancesParam iid minCount maxCount = RunInstancesParam
    { riImageId = iid
    , riMinCount = minCount
    , riMaxCount = maxCount
    , riKeyName = Nothing
    , riSecurityGroupIds = []
    , riSecurityGroups = []
    , riUserData = Nothing
    , riInstanceType = Nothing
    , riAvailabilityZone = Nothing
    , riPlacementGroup = Nothing
    , riTenancy = Nothing
    , riKernelId = Nothing
    , riRamdiskId = Nothing
    , riBlockDeviceMappings = []
    , riMonitoringEnabled = Nothing
    , riSubnetId = Nothing
    , riDisableApiTermination = Nothing
    , riShutdownBehavior = Nothing
    , riPrivateIpAddresses = []
    , riClientToken = Nothing
    , riNetworkInterface = []
    , riIamInstanceProfile = Nothing
    , riEbsOptimized = Nothing
    }

sbToText :: ShutdownBehavior -> Text
sbToText SBStop      = "stop"
sbToText SBTerminate = "terminate"

------------------------------------------------------------
-- GetConsoleOutput
------------------------------------------------------------
getConsoleOutput
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ InstanceId
    -> EC2 m ConsoleOutput
getConsoleOutput iid =
    ec2Query "GetConsoleOutput" [ValueParam "InstanceId" iid] $
        consoleOutput
        <$> getT "instanceId"
        <*> getF "timestamp" textToTime
        <*> getT "output"

------------------------------------------------------------
-- GetPasswordData
------------------------------------------------------------
getPasswordData
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ InstanceId
    -> EC2 m PasswordData
getPasswordData iid =
    ec2Query "GetPasswordData" [ValueParam "InstanceId" iid] $
        passwordData
        <$> getT "instanceId"
        <*> getF "timestamp" textToTime
        <*> getT "passwordData"
