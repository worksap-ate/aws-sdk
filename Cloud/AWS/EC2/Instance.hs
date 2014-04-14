{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Cloud.AWS.EC2.Instance
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
    , monitorInstances
    , unmonitorInstances
    , describeSpotInstanceRequests
    , requestSpotInstances
    , defaultRequestSpotInstancesParam
    , cancelSpotInstanceRequests
    ) where

import Data.Text (Text)
import Data.Conduit
import Control.Applicative
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as Map
import Control.Monad

import Cloud.AWS.Lib.Parser.Unordered (XmlElement, (.<), elementM, element, ElementPath)

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Params
import Cloud.AWS.EC2.Query
import Cloud.AWS.Lib.FromText (fromText)
import Cloud.AWS.Lib.ToText (toText)

------------------------------------------------------------
-- DescribeInstances
------------------------------------------------------------
describeInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ InstanceIds
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m Reservation)
describeInstances instances filters = do
    ec2QuerySource "DescribeInstances" params path $
        itemConduit reservationConv
  where
    path = itemsPath "reservationSet"
    params =
        [ "InstanceId" |.#= instances
        , filtersParam filters
        ]

reservationConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m Reservation
reservationConv xml = Reservation
    <$> xml .< "reservationId"
    <*> xml .< "ownerId"
    <*> groupSetConv xml
    <*> instanceSetConv xml
    <*> xml .< "requesterId"

instanceSetConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m [Instance]
instanceSetConv = itemsSet "instancesSet" conv
  where
    placementConv e = Placement
        <$> e .< "availabilityZone"
        <*> e .< "groupName"
        <*> e .< "tenancy"
    profileConv e = IamInstanceProfile
        <$> e .< "arn"
        <*> e .< "id"
    conv e = Instance
        <$> e .< "instanceId"
        <*> e .< "imageId"
        <*> instanceStateConv "instanceState" e
        <*> e .< "privateDnsName"
        <*> e .< "dnsName"
        <*> e .< "reason"
        <*> e .< "keyName"
        <*> e .< "amiLaunchIndex"
        <*> productCodeConv e
        <*> e .< "instanceType"
        <*> e .< "launchTime"
        <*> element "placement" placementConv e
        <*> e .< "kernelId"
        <*> e .< "ramdiskId"
        <*> e .< "platform"
        <*> element "monitoring" (.< "state") e
        <*> e .< "subnetId"
        <*> e .< "vpcId"
        <*> e .< "privateIpAddress"
        <*> e .< "ipAddress"
        <*> e .< "sourceDestCheck"
        <*> groupSetConv e
        <*> stateReasonConv e
        <*> e .< "architecture"
        <*> e .< "rootDeviceType"
        <*> e .< "rootDeviceName"
        <*> instanceBlockDeviceMappingsConv e
        <*> e .< "instanceLifecycle"
        <*> e .< "spotInstanceRequestId"
        <*> e .< "virtualizationType"
        <*> e .< "clientToken"
        <*> resourceTagConv e
        <*> e .< "hypervisor"
        <*> networkInterfaceConv e
        <*> elementM "iamInstanceProfile" profileConv e
        <*> e .< "ebsOptimized"

instanceBlockDeviceMappingsConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m [InstanceBlockDeviceMapping]
instanceBlockDeviceMappingsConv = itemsSet "blockDeviceMapping" conv
  where
    ebsConv e = EbsInstanceBlockDevice
        <$> e .< "volumeId"
        <*> e .< "status"
        <*> e .< "attachTime"
        <*> e .< "deleteOnTermination"
    conv e = InstanceBlockDeviceMapping
        <$> e .< "deviceName"
        <*> element "ebs" ebsConv e

instanceStateCodes :: [(Int, InstanceState)]
instanceStateCodes =
    [ ( 0, InstanceStatePending)
    , (16, InstanceStateRunning)
    , (32, InstanceStateShuttingDown)
    , (48, InstanceStateTerminated)
    , (64, InstanceStateStopping)
    , (80, InstanceStateStopped)
    ]

codeToState :: Int -> Text -> InstanceState
codeToState code _name = fromMaybe
    (InstanceStateUnknown code)
    (lookup code instanceStateCodes)

instanceStateConv :: (MonadThrow m, Applicative m)
    => Text -> XmlElement -> m InstanceState
instanceStateConv label = element label conv
  where
    conv e = codeToState
        <$> e .< "code"
        <*> e .< "name"

networkInterfaceConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m [InstanceNetworkInterface]
networkInterfaceConv = itemsSet "networkInterfaceSet" conv
  where
    attachmentConv e = InstanceNetworkInterfaceAttachment
        <$> e .< "attachmentId"
        <*> e .< "deviceIndex"
        <*> e .< "status"
        <*> e .< "attachTime"
        <*> e .< "deleteOnTermination"
    ipConv e = InstancePrivateIpAddress
        <$> e .< "privateIpAddress"
        <*> e .< "privateDnsName"
        <*> e .< "primary"
        <*> instanceNetworkInterfaceAssociationConv e
    conv e = InstanceNetworkInterface
        <$> e .< "networkInterfaceId"
        <*> e .< "subnetId"
        <*> e .< "vpcId"
        <*> e .< "description"
        <*> e .< "ownerId"
        <*> e .< "status"
        <*> e .< "macAddress"
        <*> e .< "privateIpAddress"
        <*> e .< "privateDnsName"
        <*> e .< "sourceDestCheck"
        <*> groupSetConv e
        <*> elementM "attachment" attachmentConv e
        <*> instanceNetworkInterfaceAssociationConv e
        <*> itemsSet "privateIpAddressesSet" ipConv e

instanceNetworkInterfaceAssociationConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m (Maybe InstanceNetworkInterfaceAssociation)
instanceNetworkInterfaceAssociationConv = elementM "association" conv
  where
    conv e = InstanceNetworkInterfaceAssociation
        <$> e .< "publicIp"
        <*> e .< "publicDnsName"
        <*> e .< "ipOwnerId"

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
    ec2QuerySource' "DescribeInstanceStatus" params token path $
        itemConduit instanceStatusConv
  where
    path = itemsPath "instanceStatusSet"
    params =
        [ "InstanceId" |.#= instanceIds
        , "IncludeAllInstances" |= isAll
        , filtersParam filters
        ]

instanceStatusConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m InstanceStatus
instanceStatusConv = conv
  where
    conv e = InstanceStatus
        <$> e .< "instanceId"
        <*> e .< "availabilityZone"
        <*> itemsSet "eventsSet" eventConv e
        <*> instanceStateConv "instanceState" e
        <*> instanceStatusTypeConv "systemStatus" e
        <*> instanceStatusTypeConv "instanceStatus" e
    eventConv e = InstanceStatusEvent
        <$> e .< "code"
        <*> e .< "description"
        <*> e .< "notBefore"
        <*> e .< "notAfter"

instanceStatusTypeConv :: (MonadThrow m, Applicative m)
    => Text -> XmlElement -> m InstanceStatusType
instanceStatusTypeConv name = element name conv
  where
    detailConv e = InstanceStatusDetail
        <$> e .< "name"
        <*> e .< "status"
        <*> e .< "impairedSince"
    conv e = InstanceStatusType
        <$> e .< "status"
        <*> itemsSet "details" detailConv e

------------------------------------------------------------
-- StartInstances
------------------------------------------------------------
startInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ InstanceIds
    -> EC2 m (ResumableSource m InstanceStateChange)
startInstances instanceIds =
    ec2QuerySource "StartInstances" params
        instanceStateChangePath instanceStateChangeSet
  where
    params = ["InstanceId" |.#= instanceIds]

instanceStateChangePath :: ElementPath
instanceStateChangePath = itemsPath "instancesSet"

instanceStateChangeSet
    :: (MonadResource m, MonadBaseControl IO m)
    => Conduit XmlElement m InstanceStateChange
instanceStateChangeSet = itemConduit conv
  where
    conv e = InstanceStateChange
        <$> e .< "instanceId"
        <*> instanceStateConv "currentState" e
        <*> instanceStateConv "previousState" e

------------------------------------------------------------
-- StopInstances
------------------------------------------------------------
stopInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ InstanceIds
    -> Bool -- ^ Force
    -> EC2 m (ResumableSource m InstanceStateChange)
stopInstances instanceIds force =
    ec2QuerySource "StopInstances" params
        instanceStateChangePath instanceStateChangeSet
  where
    params =
        [ "InstanceId" |.#= instanceIds
        , "Force" |= force]

------------------------------------------------------------
-- RebootInstances
------------------------------------------------------------
rebootInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ InstanceIds
    -> EC2 m Bool
rebootInstances instanceIds =
    ec2Query "RebootInstances" params (.< "return")
  where
    params = ["InstanceId" |.#= instanceIds]

------------------------------------------------------------
-- TerminateInstances
------------------------------------------------------------
terminateInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ InstanceIds
    -> EC2 m (ResumableSource m InstanceStateChange)
terminateInstances instanceIds =
    ec2QuerySource "TerminateInstances" params
        instanceStateChangePath instanceStateChangeSet
  where
    params = ["InstanceId" |.#= instanceIds]

------------------------------------------------------------
-- RunInstances
------------------------------------------------------------
-- | 'RunInstancesParam' is genereted with 'defaultRunInstancesParam'
runInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => RunInstancesRequest
    -> EC2 m Reservation
runInstances param =
    ec2Query "RunInstances" params $ reservationConv
  where
    params =
        [ "ImageId" |= runInstancesRequestImageId param
        , "MinCount" |= runInstancesRequestMinCount param
        , "MaxCount" |= runInstancesRequestMaxCount param
        , "KeyName" |=? runInstancesRequestKeyName param
        , "SecurityGroupId" |.#= runInstancesRequestSecurityGroupIds param
        , "SecurityGroup" |.#= runInstancesRequestSecurityGroups param
        , "UserData" |=? runInstancesRequestUserData param
        , "InstanceType" |=? runInstancesRequestInstanceType param
        , "Placement" |.
            [ "AvailabilityZone" |=?
                runInstancesRequestAvailabilityZone param
            , "GroupName" |=?
                runInstancesRequestPlacementGroup param
            , "Tenancy" |=?
                runInstancesRequestTenancy param
            ]
        , "KernelId" |=? runInstancesRequestKernelId param
        , "RamdiskId" |=? runInstancesRequestRamdiskId param
        , blockDeviceMappingsParam $
            runInstancesRequestBlockDeviceMappings param
        , "Monitoring" |.+ "Enabled" |=?
            runInstancesRequestMonitoringEnabled param
        , "SubnetId" |=? runInstancesRequestSubnetId param
        , "DisableApiTermination" |=?
            runInstancesRequestDisableApiTermination param
        , "InstanceInitiatedShutdownBehavior" |=?
            runInstancesRequestShutdownBehavior param
        , "PrivateIpAddress" |=?
            runInstancesRequestPrivateIpAddress param
        , "ClientToken" |=? runInstancesRequestClientToken param
        , "NetworkInterface" |.#. map networkInterfaceParams
            (runInstancesRequestNetworkInterfaces param)
        , "IamInstanceProfile" |.? iamInstanceProfileParams <$>
            runInstancesRequestIamInstanceProfile param
        , "EbsOptimized" |=?
            runInstancesRequestEbsOptimized param
        ]
    iamInstanceProfileParams iam =
        [ "Arn" |= iamInstanceProfileArn iam
        , "Name" |= iamInstanceProfileId iam
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

networkInterfaceParams :: NetworkInterfaceParam -> [QueryParam]
networkInterfaceParams (NetworkInterfaceParamCreate di si d pia pias sgi dot) =
    [ "DeviceIndex" |= di
    , "SubnetId" |= si
    , "Description" |= d
    , "PrivateIpAddress" |=? pia
    , "SecurityGroupId" |.#= sgi
    , "DeleteOnTermination" |= dot
    ] ++ s pias
  where
    s SecondaryPrivateIpAddressParamNothing = []
    s (SecondaryPrivateIpAddressParamCount c) =
        ["SecondaryPrivateIpAddressCount" |= c]
    s (SecondaryPrivateIpAddressParamSpecified addrs pr) =
        [ privateIpAddressesParam "PrivateIpAddresses" addrs
        , maybeParam $ ipAddressPrimaryParam <$> pr
        ]
    ipAddressPrimaryParam i =
        "PrivateIpAddresses" |.+ toText i |.+ "Primary" |= True
networkInterfaceParams (NetworkInterfaceParamAttach nid idx dot) =
    [ "NetworkInterfaceId" |= nid
    , "DeviceIndex" |= idx
    , "DeleteOnTermination" |= dot
    ]

------------------------------------------------------------
-- GetConsoleOutput
------------------------------------------------------------
getConsoleOutput
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ InstanceId
    -> EC2 m ConsoleOutput
getConsoleOutput iid =
    ec2Query "GetConsoleOutput" ["InstanceId" |= iid] $ \e ->
        ConsoleOutput
        <$> e .< "instanceId"
        <*> e .< "timestamp"
        <*> e .< "output"

------------------------------------------------------------
-- GetPasswordData
------------------------------------------------------------
getPasswordData
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ InstanceId
    -> EC2 m PasswordData
getPasswordData iid =
    ec2Query "GetPasswordData" ["InstanceId" |= iid] $ \e ->
        PasswordData
        <$> e .< "instanceId"
        <*> e .< "timestamp"
        <*> e .< "passwordData"

describeInstanceAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ InstanceId
    -> InstanceAttributeRequest -- ^ Attribute
    -> EC2 m InstanceAttribute
describeInstanceAttribute iid attr =
    ec2Query "DescribeInstanceAttribute" params $ f attr
  where
    str = iar attr
    params =
        [ "InstanceId" |= iid
        , "Attribute" |= str
        ]
    f InstanceAttributeRequestBlockDeviceMapping = instanceBlockDeviceMappingsConv
        >=> return . InstanceAttributeBlockDeviceMapping
    f InstanceAttributeRequestProductCodes =
        productCodeConv >=> return . InstanceAttributeProductCodes
    f InstanceAttributeRequestGroupSet =
        itemsSet str (.< "groupId")
        >=> return . InstanceAttributeGroupSet
    f req = valueConv str (fromJust $ Map.lookup req h)
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
           . fromJust . fromText . fromJust)
        , (InstanceAttributeRequestRootDeviceName,
           InstanceAttributeRootDeviceName)
        , (InstanceAttributeRequestSourceDestCheck,
           InstanceAttributeSourceDestCheck
           . fromText . fromJust)
        , (InstanceAttributeRequestEbsOptimized,
           InstanceAttributeEbsOptimized . just)
        ]
    just = fromJust . join . (fromText <$>)
    valueConv name val =
        element name (.< "value") >=> return . val

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
    ec2Query "ResetInstanceAttribute" params (.< "return")
  where
    params =
        [ "InstanceId" |= iid
        , "Attribute" |= riap attr
        ]

-- | not tested
modifyInstanceAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ InstanceId
    -> ModifyInstanceAttributeRequest
    -> EC2 m Bool
modifyInstanceAttribute iid attr =
    ec2Query "ModifyInstanceAttribute" params (.< "return")
  where
    params = ["InstanceId" |= iid, miap attr]

miap :: ModifyInstanceAttributeRequest -> QueryParam
miap (ModifyInstanceAttributeRequestInstanceType a) =
    "InstanceType" |.+ "Value" |= a
miap (ModifyInstanceAttributeRequestKernelId a) =
    "Kernel" |.+ "Value" |= a
miap (ModifyInstanceAttributeRequestRamdiskId a) =
    "Ramdisk" |.+ "Value" |= a
miap (ModifyInstanceAttributeRequestUserData a) =
    "UserData" |.+ "Value" |= a
miap (ModifyInstanceAttributeRequestDisableApiTermination a) =
    "DisableApiTermination" |.+ "Value" |= a
miap (ModifyInstanceAttributeRequestShutdownBehavior a) =
    "InstanceInitiatedShutdownBehavior" |.+ "Value" |= a
miap (ModifyInstanceAttributeRequestRootDeviceName a) =
    "RootDeviceName" |= a
miap (ModifyInstanceAttributeRequestBlockDeviceMapping a) =
    blockDeviceMappingsParam a
miap (ModifyInstanceAttributeRequestSourceDestCheck a) =
    "SourceDestCheck" |.+ "Value" |= a
miap (ModifyInstanceAttributeRequestGroupSet a) =
    "GroupId" |.#= a
miap (ModifyInstanceAttributeRequestEbsOptimized a) =
    "EbsOptimized" |= a

------------------------------------------------------------
-- MonitorInstances
------------------------------------------------------------
monitorInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ InstanceIds
    -> EC2 m (ResumableSource m MonitorInstancesResponse)
monitorInstances iids =
    ec2QuerySource "MonitorInstances" ["InstanceId" |.#= iids]
        (itemsPath "instancesSet") monitorInstancesResponseConv

monitorInstancesResponseConv
    :: (MonadResource m, MonadBaseControl IO m)
    => Conduit XmlElement m MonitorInstancesResponse
monitorInstancesResponseConv = itemConduit $ \e ->
    MonitorInstancesResponse
    <$> e .< "instanceId"
    <*> element "monitoring" (.< "state") e

------------------------------------------------------------
-- UnmonitorInstances
------------------------------------------------------------
unmonitorInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ InstanceIds
    -> EC2 m (ResumableSource m MonitorInstancesResponse)
unmonitorInstances iids =
    ec2QuerySource "UnmonitorInstances" ["InstanceId" |.#= iids]
        (itemsPath "instancesSet") monitorInstancesResponseConv

------------------------------------------------------------
-- DescribeSpotInstanceRequests
------------------------------------------------------------
describeSpotInstanceRequests
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ SpotInstanceRequestIds
    -> [Filter] -- ^ Filters
    -> EC2 m (ResumableSource m SpotInstanceRequest)
describeSpotInstanceRequests requests filters = do
--    ec2QueryDebug "DescribeInstances" params
    ec2QuerySource "DescribeSpotInstanceRequests" params path $
        itemConduit spotInstanceRequestConv
  where
    path = itemsPath "spotInstanceRequestSet"
    params =
        [ "SpotInstanceRequestId" |.#= requests
        , filtersParam filters
        ]

spotInstanceRequestConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m SpotInstanceRequest
spotInstanceRequestConv = conv
  where
    conv e = SpotInstanceRequest
        <$> e .< "spotInstanceRequestId"
        <*> e .< "spotPrice"
        <*> e .< "type"
        <*> e .< "state"
        <*> elementM "fault" faultConv e
        <*> element "status" statusConv e
        <*> e .< "validFrom"
        <*> e .< "validUntil"
        <*> e .< "launchGroup"
        <*> e .< "availabilityZoneGroup"
        <*> spotInstanceLaunchSpecificationConv "launchSpecification" e
        <*> e .< "instanceId"
        <*> e .< "createTime"
        <*> e .< "productDescription"
        <*> resourceTagConv e
        <*> e .< "launchedAvailabilityZone"
    faultConv e = SpotInstanceFault
        <$> e .< "code"
        <*> e .< "message"
    statusConv e = SpotInstanceStatus
        <$> e .< "code"
        <*> e .< "updateTime"
        <*> e .< "message"


spotInstanceLaunchSpecificationConv :: (MonadThrow m, Applicative m)
    => Text -> XmlElement -> m SpotInstanceLaunchSpecification
spotInstanceLaunchSpecificationConv label = element label conv
  where
    conv e = SpotInstanceLaunchSpecification
        <$> e .< "imageId"
        <*> e .< "keyName"
        <*> groupSetConv e
        <*> e .< "instanceType"
        <*> element "placement" plConv e
        <*> e .< "kernelId"
        <*> e .< "ramdiskId"
        <*> spotInstanceBlockDeviceMappingsConv e
        <*> element "monitoring" monConv e
        <*> e .< "subnetId"
        <*> spotInstanceNetworkInterfaceConv e
        <*> elementM "iamInstanceProfile" profConv e
        <*> e .< "ebsOptimized"
    plConv e = Placement
        <$> e .< "availabilityZone"
        <*> e .< "groupName"
        <*> e .< "tenancy"
    profConv e = IamInstanceProfile
        <$> e .< "arn"
        <*> e .< "id"
    monConv e = SpotInstanceMonitoringState
        <$> e .< "enabled"


spotInstanceBlockDeviceMappingsConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m [SpotInstanceBlockDeviceMapping]
spotInstanceBlockDeviceMappingsConv = itemsSet "blockDeviceMapping" conv
  where
    conv e = SpotInstanceBlockDeviceMapping
        <$> e .< "deviceName"
        <*> element "ebs" ebsConv e
    ebsConv e = EbsSpotInstanceBlockDevice
        <$> e .< "volumeSize"
        <*> e .< "deleteOnTermination"
        <*> e .< "volumeType"

spotInstanceNetworkInterfaceConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m [SpotInstanceNetworkInterface]
spotInstanceNetworkInterfaceConv = itemsSet "networkInterfaceSet" conv
  where
    conv e = SpotInstanceNetworkInterface
        <$> e .< "deviceIndex"
        <*> e .< "subnetId"
        <*> securityGroupSetConv e

securityGroupSetConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m [SpotInstanceSecurityGroup]
securityGroupSetConv = itemsSet "groupSet" conv
  where
    conv e = SpotInstanceSecurityGroup
        <$> e .< "groupId"

------------------------------------------------------------
-- RequestSpotInstances
------------------------------------------------------------
-- | 'RequestSpotInstancesParam' is genereted with 'defaultRequestSpotInstancesParam'
requestSpotInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => RequestSpotInstancesParam
    -> EC2 m [SpotInstanceRequest]
requestSpotInstances param =
    ec2Query "RequestSpotInstances" params $
        itemsSet "spotInstanceRequestSet" spotInstanceRequestConv
  where
    params =
        [ "SpotPrice" |= requestSpotInstancesSpotPrice param
        , "InstanceCount" |=? requestSpotInstancesCount param
        , "Type" |=? requestSpotInstancesType param
        , "ValidFrom" |=? requestSpotInstancesValidFrom param
        , "ValidUntil" |=? requestSpotInstancesValidUntil param
        , "LaunchGroup" |=? requestSpotInstancesLaunchGroup param
        , "AvailabilityZoneGroup" |=? requestSpotInstancesLaunchGroup param
        , "LaunchSpecification" |.
          [ "ImageId" |= requestSpotInstancesImageId param
          , "KeyName" |=? requestSpotInstancesKeyName param
          , "SecurityGroupId" |.#= requestSpotInstancesSecurityGroupIds param
          , "SecurityGroup" |.#= requestSpotInstancesSecurityGroups param
          , "UserData" |=? requestSpotInstancesUserData param
          , "InstanceType" |= requestSpotInstancesInstanceType param
          , "Placement" |.
              [ "AvailabilityZone" |=?
                  requestSpotInstancesAvailabilityZone param
              , "GroupName" |=?
                  requestSpotInstancesPlacementGroup param
              ]
          , "KernelId" |=? requestSpotInstancesKernelId param
          , "RamdiskId" |=? requestSpotInstancesRamdiskId param
          , blockDeviceMappingsParam $
              requestSpotInstancesBlockDeviceMappings param
          , "Monitoring" |.+ "Enabled" |=?
              requestSpotInstancesMonitoringEnabled param
          , "SubnetId" |=? requestSpotInstancesSubnetId param
          , "NetworkInterface" |.#. map networkInterfaceParams
              (requestSpotInstancesNetworkInterfaces param)
          , "IamInstanceProfile" |.? iamInstanceProfileParams <$>
              requestSpotInstancesIamInstancesProfile param
          , "EbsOptimized" |=?
              requestSpotInstancesEbsOptimized param
          ]
        ]
    iamInstanceProfileParams iam =
        [ "Arn" |= iamInstanceProfileArn iam
        , "Name" |= iamInstanceProfileId iam
        ]

-- | RequestSpotInstances parameter utility
defaultRequestSpotInstancesParam
    :: Text -- ^ Price
    -> Text -- ^ ImageId
    -> Text -- ^ Instance type
    -> RequestSpotInstancesParam
defaultRequestSpotInstancesParam price iid iType
    = RequestSpotInstancesParam
        price
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        iid
        Nothing
        []
        []
        Nothing
        iType
        Nothing
        Nothing
        Nothing
        Nothing
        []
        Nothing
        Nothing
        []
        Nothing
        Nothing

------------------------------------------------------------
-- CancelSpotInstanceRequests
------------------------------------------------------------
cancelSpotInstanceRequests
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ InstanceIds
    -> EC2 m (ResumableSource m CancelSpotInstanceRequestsResponse)
cancelSpotInstanceRequests requestIds =
    ec2QuerySource "CancelSpotInstanceRequests" params path $
        itemConduit cancelSpotInstanceResponseConv
  where
    path = itemsPath "spotInstanceRequestSet"
    params = ["SpotInstanceRequestId" |.#= requestIds]

cancelSpotInstanceResponseConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m CancelSpotInstanceRequestsResponse
cancelSpotInstanceResponseConv e = CancelSpotInstanceRequestsResponse
    <$> e .< "spotInstanceRequestId"
    <*> e .< "state"
