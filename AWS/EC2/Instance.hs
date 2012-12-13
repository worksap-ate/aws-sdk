{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.EC2.Instance
    ( describeInstances
    , runInstances
    , defaultRunInstancesRequest
    , terminateInstances
    , startInstances
    , stopInstances
    , rebootInstances
    , getConsoleOutput
    , getPasswordData
    , describeInstanceStatus
    , describeInstanceAttribute
    , resetInstanceAttribute
    , modifyInstanceAttribute
    ) where

import Data.Text (Text)
import Data.XML.Types (Event)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Data.Monoid
import Control.Monad

import AWS.EC2.Convert
import AWS.EC2.Internal
import AWS.EC2.Types
import AWS.EC2.Params
import AWS.EC2.Query
import AWS.Lib.Parser
import AWS.Util

------------------------------------------------------------
-- DescribeInstances
------------------------------------------------------------
describeInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ InstanceIds
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m Reservation)
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
    Reservation
    <$> getT "reservationId"
    <*> getT "ownerId"
    <*> groupSetSink
    <*> instanceSetSink
    <*> getMT "requesterId"

instanceSetSink :: MonadThrow m
    => GLSink Event m [Instance]
instanceSetSink = itemsSet "instancesSet" $
    Instance
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
    <*> getT "launchTime"
    <*> element "placement" (
        Placement
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
    <*> getMT "sourceDestCheck"
    <*> groupSetSink
    <*> stateReasonSink
    <*> getF "architecture" architecture
    <*> getF "rootDeviceType" rootDeviceType
    <*> getMT "rootDeviceName"
    <*> instanceBlockDeviceMappingsSink
    <*> getM "instanceLifecycle" instanceLifecycle
    <*> getMT "spotInstanceRequestId"
    <*> getF "virtualizationType" virtualizationType
    <*> getT "clientToken"
    <*> resourceTagSink
    <*> getF "hypervisor" hypervisor
    <*> networkInterfaceSink
    <*> elementM "iamInstanceProfile" (
        IamInstanceProfile
        <$> getT "arn"
        <*> getT "id"
        )
    <*> getT "ebsOptimized"

instanceBlockDeviceMappingsSink :: MonadThrow m
    => GLSink Event m [InstanceBlockDeviceMapping]
instanceBlockDeviceMappingsSink = itemsSet "blockDeviceMapping" (
    InstanceBlockDeviceMapping
    <$> getT "deviceName"
    <*> element "ebs" (
        EbsInstanceBlockDevice
        <$> getT "volumeId"
        <*> getF "status" attachmentSetItemResponseStatus'
        <*> getT "attachTime"
        <*> getT "deleteOnTermination"
        )
    )

instanceStateSink :: MonadThrow m
    => Text -> GLSink Event m InstanceState
instanceStateSink label = element label $
    codeToState
    <$> getT "code"
    <*> getT "name"

networkInterfaceSink :: MonadThrow m
    => GLSink Event m [InstanceNetworkInterface]
networkInterfaceSink = itemsSet "networkInterfaceSet" $
    InstanceNetworkInterface
    <$> getT "networkInterfaceId"
    <*> getT "subnetId"
    <*> getT "vpcId"
    <*> getM "description" orEmpty
    <*> getT "ownerId"
    <*> getT "status"
    <*> getT "privateIpAddress"
    <*> getMT "privateDnsName"
    <*> getT "sourceDestCheck"
    <*> groupSetSink
    <*> element "attachment" (
        InstanceNetworkInterfaceAttachment
        <$> getT "attachmentId"
        <*> getT "deviceIndex"
        <*> getT "status"
        <*> getT "attachTime"
        <*> getT "deleteOnTermination"
        )
    <*> instanceNetworkInterfaceAssociationSink
    <*> itemsSet "privateIpAddressesSet" (
        InstancePrivateIpAddress
        <$> getT "privateIpAddress"
        <*> getT "primary"
        <*> instanceNetworkInterfaceAssociationSink
        )

instanceNetworkInterfaceAssociationSink :: MonadThrow m
    => GLSink Event m (Maybe InstanceNetworkInterfaceAssociation)
instanceNetworkInterfaceAssociationSink = elementM "association" $
    InstanceNetworkInterfaceAssociation
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
    -> EC2 m (ResumableSource m InstanceStatus)
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
        InstanceStatus
        <$> getT "instanceId"
        <*> getT "availabilityZone"
        <*> itemsSet "eventsSet" (
            InstanceStatusEvent
            <$> getF "code" instanceStatusEventCode'
            <*> getT "description"
            <*> getMT "notBefore"
            <*> getMT "notAfter"
            )
        <*> instanceStateSink "instanceState"
        <*> instanceStatusTypeSink "systemStatus"
        <*> instanceStatusTypeSink "instanceStatus"

instanceStatusTypeSink :: MonadThrow m
    => Text -> GLSink Event m InstanceStatusType
instanceStatusTypeSink name = element name $
    InstanceStatusType
    <$> getF "status" instanceStatusTypeStatus'
    <*> itemsSet "details" (
        InstanceStatusDetail
        <$> getT "name"
        <*> getT "status"
        <*> getMT "impairedSince"
        )

------------------------------------------------------------
-- StartInstances
------------------------------------------------------------
startInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ InstanceIds
    -> EC2 m (ResumableSource m InstanceStateChange)
startInstances instanceIds =
    ec2QuerySource "StartInstances" params instanceStateChangeSet
  where
    params = [ArrayParams "InstanceId" instanceIds]

instanceStateChangeSet
    :: (MonadResource m, MonadBaseControl IO m)
    => Conduit Event m InstanceStateChange
instanceStateChangeSet = itemConduit "instancesSet" $ do
    InstanceStateChange
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
    -> EC2 m (ResumableSource m InstanceStateChange)
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
    ec2Query "RebootInstances" params $ getT "return"
  where
    params = [ArrayParams "InstanceId" instanceIds]

------------------------------------------------------------
-- TerminateInstances
------------------------------------------------------------
terminateInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ InstanceIds
    -> EC2 m (ResumableSource m InstanceStateChange)
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
    => RunInstancesRequest
    -> EC2 m Reservation
runInstances param =
    ec2Query "RunInstances" params reservationSink
  where
    params =
        [ ValueParam "ImageId" $ runInstancesRequestImageId param
        , ValueParam "MinCount"
            $ toText $ runInstancesRequestMinCount param
        , ValueParam "MaxCount"
            $ toText $ runInstancesRequestMaxCount param
        , ArrayParams "SecurityGroupId"
            $ runInstancesRequestSecurityGroupIds param
        , ArrayParams "SecurityGroup"
            $ runInstancesRequestSecurityGroups param
        , blockDeviceMappingParams
            $ runInstancesRequestBlockDeviceMappings param
        ] ++ networkInterfaceParams
             (runInstancesRequestNetworkInterfaces param)
          ++ maybeParams
            [ ("KeyName", runInstancesRequestKeyName param)
            , ("UserData"
              , bsToText <$> runInstancesRequestUserData param
              )
            , ("InstanceType"
              , runInstancesRequestInstanceType param
              )
            , ("Placement.AvailabilityZone"
              , runInstancesRequestAvailabilityZone param
              )
            , ("Placement.GroupName"
              , runInstancesRequestPlacementGroup param
              )
            , ("Placement.Tenancy"
              , runInstancesRequestTenancy param
              )
            , ("KernelId", runInstancesRequestKernelId param)
            , ("RamdiskId", runInstancesRequestRamdiskId param)
            , ("Monitoring.Enabled"
              , boolToText
                <$> runInstancesRequestMonitoringEnabled param
              )
            , ("SubnetId", runInstancesRequestSubnetId param)
            , ("DisableApiTermination"
              , boolToText
                <$> runInstancesRequestDisableApiTermination param
              )
            , ("InstanceInitiatedShutdownBehavior"
              , sbToText
                <$> runInstancesRequestShutdownBehavior param
              )
            , ("PrivateIpAddress"
              , toText
                <$> runInstancesRequestPrivateIpAddress param
              )
            , ("ClientToken", runInstancesRequestClientToken param)
            , ("IamInstanceProfile.Arn"
              , iamInstanceProfileArn
                <$> runInstancesRequestIamInstanceProfile param
              )
            , ("IamInstanceProfile.Name"
              , iamInstanceProfileId
                <$> runInstancesRequestIamInstanceProfile param
              )
            , ("EbsOptimized"
              , boolToText
                <$> runInstancesRequestEbsOptimized param
              )
            ]

-- | RunInstances parameter utility
defaultRunInstancesRequest
    :: Text -- ^ ImageId
    -> Int -- ^ MinCount
    -> Int -- ^ MaxCount
    -> RunInstancesRequest
defaultRunInstancesRequest iid minCount maxCount
    = RunInstancesRequest
        iid
        minCount
        maxCount
        Nothing
        []
        []
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        []
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        []
        Nothing
        Nothing

networkInterfaceParams :: [NetworkInterfaceParam] -> [QueryParam]
networkInterfaceParams nips = f 1 nips
  where
    f :: Int -> [NetworkInterfaceParam] -> [QueryParam]
    f _ [] = []
    f n (ni:nis) = g n ni ++ f (n + 1) nis
    g n (NetworkInterfaceParamAttach nid idx dot) =
        [ ValueParam (p n "NetworkInterfaceId") nid
        , ValueParam (p n "DeviceIndex") $ toText idx
        , ValueParam (p n "DeleteOnTermination") $ boolToText dot
        ]
    g n (NetworkInterfaceParamCreate idx sn desc pip sip sec dot) =
        [ ValueParam (p n "DeviceIndex") $ toText idx
        , ValueParam (p n "SubnetId") sn
        , ValueParam (p n "Description") desc
        , ArrayParams (p n "SecurityroupId") sec
        , ValueParam (p n "DeleteOnTermination") $ boolToText dot
        ] ++ maybeParams [(p n "PrivateIpAddress", toText <$> pip)]
          ++ s n sip
    p n name = "NetworkInterface." <> toText n <> "." <> name
    s _ SecondaryPrivateIpAddressParamNothing = []
    s n (SecondaryPrivateIpAddressParamCount c) =
        [ ValueParam
          (p n "SecondaryPrivateIpAddressCount")
          $ toText c
        ]
    s n (SecondaryPrivateIpAddressParamSpecified addrs pr) =
        [ privateIpAddressesParam (p n "PrivateIpAddresses") addrs
        ] ++ maybe
            []
            (\i -> [
              ValueParam
              (p n "PrivateIpAddresses." <> toText i <> ".Primary")
              "true"])
            pr

sbToText :: ShutdownBehavior -> Text
sbToText ShutdownBehaviorStop      = "stop"
sbToText ShutdownBehaviorTerminate = "terminate"

------------------------------------------------------------
-- GetConsoleOutput
------------------------------------------------------------
getConsoleOutput
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ InstanceId
    -> EC2 m ConsoleOutput
getConsoleOutput iid =
    ec2Query "GetConsoleOutput" [ValueParam "InstanceId" iid] $
        ConsoleOutput
        <$> getT "instanceId"
        <*> getT "timestamp"
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
        PasswordData
        <$> getT "instanceId"
        <*> getT "timestamp"
        <*> getT "passwordData"

describeInstanceAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ InstanceId
    -> InstanceAttributeRequest -- ^ Attribute
    -> EC2 m InstanceAttribute
describeInstanceAttribute iid attr =
    ec2Query "DescribeInstanceAttribute" params
        $ getT_ "instanceId" *> f attr
  where
    str = iar attr
    params =
        [ ValueParam "InstanceId" iid
        , ValueParam "Attribute" str
        ]
    f InstanceAttributeRequestBlockDeviceMapping = instanceBlockDeviceMappingsSink
        >>= return . InstanceAttributeBlockDeviceMapping
    f InstanceAttributeRequestProductCodes =
        productCodeSink >>= return . InstanceAttributeProductCodes
    f InstanceAttributeRequestGroupSet =
        (itemsSet str $ getT "groupId")
        >>= return . InstanceAttributeGroupSet
    f req = valueSink str (fromJust $ Map.lookup req h)
    h = Map.fromList
        [ (InstanceAttributeRequestInstanceType,
           InstanceAttributeInstanceType . fromJust)
        , (InstanceAttributeRequestKernelId, InstanceAttributeKernelId)
        , (InstanceAttributeRequestRamdiskId, InstanceAttributeRamdiskId)
        , (InstanceAttributeRequestUserData, InstanceAttributeUserData)
        , (InstanceAttributeRequestDisableApiTermination,
           InstanceAttributeDisableApiTermination . just)
        , (InstanceAttributeRequestShutdownBehavior,
           InstanceAttributeShutdownBehavior
           . shutdownBehavior . fromJust)
        , (InstanceAttributeRequestRootDeviceName,
           InstanceAttributeRootDeviceName)
        , (InstanceAttributeRequestSourceDestCheck,
           InstanceAttributeSourceDestCheck
           . fromTextMay . fromJust)
        , (InstanceAttributeRequestEbsOptimized,
           InstanceAttributeEbsOptimized . just)
        ]
    just = fromJust . join . (fromTextMay <$>)
    valueSink name val =
        (element name $ getMT "value") >>= return . val

iar :: InstanceAttributeRequest -> Text
iar InstanceAttributeRequestInstanceType          = "instanceType"
iar InstanceAttributeRequestKernelId              = "kernel"
iar InstanceAttributeRequestRamdiskId             = "ramdisk"
iar InstanceAttributeRequestUserData              = "userData"
iar InstanceAttributeRequestDisableApiTermination = "disableApiTermination"
iar InstanceAttributeRequestShutdownBehavior      = "instanceInitiatedShutdownBehavior"
iar InstanceAttributeRequestRootDeviceName        = "rootDeviceName"
iar InstanceAttributeRequestBlockDeviceMapping    = "blockDeviceMapping"
iar InstanceAttributeRequestSourceDestCheck       = "sourceDestCheck"
iar InstanceAttributeRequestGroupSet              = "groupSet"
iar InstanceAttributeRequestProductCodes          = "productCodes"
iar InstanceAttributeRequestEbsOptimized          = "ebsOptimized"

riap :: ResetInstanceAttributeRequest -> Text
riap ResetInstanceAttributeRequestKernel          = "kernel"
riap ResetInstanceAttributeRequestRamdisk         = "ramdisk"
riap ResetInstanceAttributeRequestSourceDestCheck = "sourceDestCheck"

resetInstanceAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ InstanceId
    -> ResetInstanceAttributeRequest
    -> EC2 m Bool
resetInstanceAttribute iid attr =
    ec2Query "ResetInstanceAttribute" params $ getT "return"
  where
    params =
        [ ValueParam "InstanceId" iid
        , ValueParam "Attribute" $ riap attr
        ]

-- | not tested
modifyInstanceAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ InstanceId
    -> [ModifyInstanceAttributeRequest]
    -> EC2 m Bool
modifyInstanceAttribute iid attrs =
    ec2Query "ModifyInstanceAttribute" params $ getT "return"
  where
    params = ValueParam "InstanceId" iid:concatMap miap attrs

miap :: ModifyInstanceAttributeRequest -> [QueryParam]
miap (ModifyInstanceAttributeRequestInstanceType a) =
    [ValueParam "InstanceType.Value" a]
miap (ModifyInstanceAttributeRequestKernelId a) =
    [ValueParam "Kernel.Value"  a]
miap (ModifyInstanceAttributeRequestRamdiskId a) =
    [ValueParam "Ramdisk.Value" a]
miap (ModifyInstanceAttributeRequestUserData a) =
    [ValueParam "UserData.Value" a]
miap (ModifyInstanceAttributeRequestDisableApiTermination a) =
    [ValueParam "DisableApiTermination.Value" $ toText a]
miap (ModifyInstanceAttributeRequestShutdownBehavior a) =
    [ValueParam "InstanceInitiatedShutdownBehavior.Value"
     $ sbToText a]
miap (ModifyInstanceAttributeRequestRootDeviceName a) =
    [ValueParam "RootDeviceName" a]
miap (ModifyInstanceAttributeRequestBlockDeviceMapping a) =
    [blockDeviceMappingParams a]
miap (ModifyInstanceAttributeRequestSourceDestCheck a) =
    [ValueParam "SourceDestCheck.Value" $ toText a]
miap (ModifyInstanceAttributeRequestGroupSet a) =
    [ArrayParams "GroupId" a]
miap (ModifyInstanceAttributeRequestEbsOptimized a) =
    [ValueParam "EbsOptimized" $ toText a]
