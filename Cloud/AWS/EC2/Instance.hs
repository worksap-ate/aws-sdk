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
import Data.XML.Types (Event)
import Data.Conduit
import Control.Applicative
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as Map
import Control.Monad

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Types
import Cloud.AWS.EC2.Params
import Cloud.AWS.EC2.Query
import Cloud.AWS.Lib.FromText (fromText)
import Cloud.AWS.Lib.Parser.Unordered
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
    ec2QuerySource "DescribeInstances" params $
        itemConduit "reservationSet" reservationConv
  where
    params =
        [ "InstanceId" |.#= instances
        , filtersParam filters
        ]

reservationConv :: (MonadThrow m, Applicative m)
    => SimpleXML -> m Reservation
reservationConv xml =
    Reservation
    <$> xml .< "reservationId"
    <*> xml .< "ownerId"
    <*> groupSetConv xml
    <*> instanceSetConv xml
    <*> xml .< "requesterId"

instanceSetConv :: (MonadThrow m, Applicative m)
    => SimpleXML -> m [Instance]
instanceSetConv xml = itemsSet xml "instancesSet" $ \xml' ->
    Instance
    <$> xml' .< "instanceId"
    <*> xml' .< "imageId"
    <*> instanceStateConv "instanceState" xml'
    <*> xml' .< "privateDnsName"
    <*> xml' .< "dnsName"
    <*> xml' .< "reason"
    <*> xml' .< "keyName"
    <*> xml' .< "amiLaunchIndex"
    <*> productCodeConv xml'
    <*> xml' .< "instanceType"
    <*> xml' .< "launchTime"
    <*> getElement xml' "placement" (\xml'' ->
        Placement
        <$> xml'' .< "availabilityZone"
        <*> xml'' .< "groupName"
        <*> xml'' .< "tenancy"
        )
    <*> xml' .< "kernelId"
    <*> xml' .< "ramdiskId"
    <*> xml' .< "platform"
    <*> getElement xml' "monitoring" (.< "state")
    <*> xml' .< "subnetId"
    <*> xml' .< "vpcId"
    <*> xml' .< "privateIpAddress"
    <*> xml' .< "ipAddress"
    <*> xml' .< "sourceDestCheck"
    <*> groupSetConv xml'
    <*> stateReasonConv xml'
    <*> xml' .< "architecture"
    <*> xml' .< "rootDeviceType"
    <*> xml' .< "rootDeviceName"
    <*> instanceBlockDeviceMappingsConv xml'
    <*> xml' .< "instanceLifecycle"
    <*> xml' .< "spotInstanceRequestId"
    <*> xml' .< "virtualizationType"
    <*> xml' .< "clientToken"
    <*> resourceTagConv xml'
    <*> xml' .< "hypervisor"
    <*> networkInterfaceConv xml'
    <*> getElementM xml' "iamInstanceProfile" (\xml'' ->
        IamInstanceProfile
        <$> xml'' .< "arn"
        <*> xml'' .< "id"
        )
    <*> xml' .< "ebsOptimized"

instanceBlockDeviceMappingsConv :: (MonadThrow m, Applicative m)
    => SimpleXML -> m [InstanceBlockDeviceMapping]
instanceBlockDeviceMappingsConv xml = itemsSet xml "blockDeviceMapping" (\xml' ->
    InstanceBlockDeviceMapping
    <$> xml' .< "deviceName"
    <*> getElement xml' "ebs" (\xml'' ->
        EbsInstanceBlockDevice
        <$> xml'' .< "volumeId"
        <*> xml'' .< "status"
        <*> xml'' .< "attachTime"
        <*> xml'' .< "deleteOnTermination"
        )
    )

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
    => Text -> SimpleXML -> m InstanceState
instanceStateConv label xml = getElement xml label $ \xml' ->
    codeToState
    <$> xml' .< "code"
    <*> xml' .< "name"

networkInterfaceConv :: (MonadThrow m, Applicative m)
    => SimpleXML -> m [InstanceNetworkInterface]
networkInterfaceConv xml = itemsSet xml "networkInterfaceSet" $ \xml' ->
    InstanceNetworkInterface
    <$> xml' .< "networkInterfaceId"
    <*> xml' .< "subnetId"
    <*> xml' .< "vpcId"
    <*> xml' .< "description"
    <*> xml' .< "ownerId"
    <*> xml' .< "status"
    <*> xml' .< "macAddress"
    <*> xml' .< "privateIpAddress"
    <*> xml' .< "privateDnsName"
    <*> xml' .< "sourceDestCheck"
    <*> groupSetConv xml'
    <*> getElementM xml' "attachment" (\xml'' ->
        InstanceNetworkInterfaceAttachment
        <$> xml'' .< "attachmentId"
        <*> xml'' .< "deviceIndex"
        <*> xml'' .< "status"
        <*> xml'' .< "attachTime"
        <*> xml'' .< "deleteOnTermination"
        )
    <*> instanceNetworkInterfaceAssociationConv xml'
    <*> itemsSet xml' "privateIpAddressesSet" (\xml'' ->
        InstancePrivateIpAddress
        <$> xml'' .< "privateIpAddress"
        <*> xml'' .< "privateDnsName"
        <*> xml'' .< "primary"
        <*> instanceNetworkInterfaceAssociationConv xml''
        )

instanceNetworkInterfaceAssociationConv :: (MonadThrow m, Applicative m)
    => SimpleXML -> m (Maybe InstanceNetworkInterfaceAssociation)
instanceNetworkInterfaceAssociationConv xml = getElementM xml "association" $ \xml' ->
    InstanceNetworkInterfaceAssociation
    <$> xml' .< "publicIp"
    <*> xml' .< "publicDnsName"
    <*> xml' .< "ipOwnerId"

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
        [ "InstanceId" |.#= instanceIds
        , "IncludeAllInstances" |= isAll
        , filtersParam filters
        ]

instanceStatusSet :: (MonadThrow m, Applicative m)
    => Conduit Event m InstanceStatus
instanceStatusSet = do
    itemConduit "instanceStatusSet" $ \xml ->
        InstanceStatus
        <$> xml .< "instanceId"
        <*> xml .< "availabilityZone"
        <*> itemsSet xml "eventsSet" (\xml' ->
            InstanceStatusEvent
            <$> xml' .< "code"
            <*> xml' .< "description"
            <*> xml' .< "notBefore"
            <*> xml' .< "notAfter"
            )
        <*> instanceStateConv "instanceState" xml
        <*> instanceStatusTypeConv "systemStatus" xml
        <*> instanceStatusTypeConv "instanceStatus" xml

instanceStatusTypeConv :: (MonadThrow m, Applicative m)
    => Text -> SimpleXML -> m InstanceStatusType
instanceStatusTypeConv name xml = getElement xml name $ \xml' ->
    InstanceStatusType
    <$> xml' .< "status"
    <*> itemsSet xml' "details" (\xml'' ->
        InstanceStatusDetail
        <$> xml'' .< "name"
        <*> xml'' .< "status"
        <*> xml'' .< "impairedSince"
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
    params = ["InstanceId" |.#= instanceIds]

instanceStateChangeSet
    :: (MonadResource m, MonadBaseControl IO m)
    => Conduit Event m InstanceStateChange
instanceStateChangeSet = itemConduit "instancesSet" $ \xml ->
    InstanceStateChange
    <$> xml .< "instanceId"
    <*> instanceStateConv "currentState" xml
    <*> instanceStateConv "previousState" xml

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
    ec2Query "RebootInstances" params $ xmlParser (.< "return")
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
        instanceStateChangeSet
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
    ec2Query "RunInstances" params $ xmlParser reservationConv
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
    ec2Query "GetConsoleOutput" ["InstanceId" |= iid] $
        xmlParser $ \xml ->
            ConsoleOutput
            <$> xml .< "instanceId"
            <*> xml .< "timestamp"
            <*> xml .< "output"

------------------------------------------------------------
-- GetPasswordData
------------------------------------------------------------
getPasswordData
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ InstanceId
    -> EC2 m PasswordData
getPasswordData iid =
    ec2Query "GetPasswordData" ["InstanceId" |= iid] $
        xmlParser $ \xml ->
            PasswordData
            <$> xml .< "instanceId"
            <*> xml .< "timestamp"
            <*> xml .< "passwordData"

describeInstanceAttribute
    :: (MonadResource m, MonadBaseControl IO m)
    => Text -- ^ InstanceId
    -> InstanceAttributeRequest -- ^ Attribute
    -> EC2 m InstanceAttribute
describeInstanceAttribute iid attr =
    ec2Query "DescribeInstanceAttribute" params $
        xmlParser $ f attr
  where
    str = iar attr
    params =
        [ "InstanceId" |= iid
        , "Attribute" |= str
        ]
    f InstanceAttributeRequestBlockDeviceMapping xml = instanceBlockDeviceMappingsConv xml
        >>= return . InstanceAttributeBlockDeviceMapping
    f InstanceAttributeRequestProductCodes xml =
        productCodeConv xml >>= return . InstanceAttributeProductCodes
    f InstanceAttributeRequestGroupSet xml =
        (itemsSet xml str (.< "groupId"))
        >>= return . InstanceAttributeGroupSet
    f req xml = valueConv str (fromJust $ Map.lookup req h) xml
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
    valueConv name val xml =
        (getElement xml name (.< "value")) >>= return . val

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
    ec2Query "ResetInstanceAttribute" params $ xmlParser (.< "return")
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
    ec2Query "ModifyInstanceAttribute" params $ xmlParser (.< "return")
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
        monitorInstancesResponseConv

monitorInstancesResponseConv
    :: (MonadResource m, MonadBaseControl IO m)
    => Conduit Event m MonitorInstancesResponse
monitorInstancesResponseConv = itemConduit "instancesSet" $ \xml ->
    MonitorInstancesResponse
    <$> xml .< "instanceId"
    <*> getElement xml "monitoring" (.< "state")

------------------------------------------------------------
-- UnmonitorInstances
------------------------------------------------------------
unmonitorInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => [Text] -- ^ InstanceIds
    -> EC2 m (ResumableSource m MonitorInstancesResponse)
unmonitorInstances iids =
    ec2QuerySource "UnmonitorInstances" ["InstanceId" |.#= iids]
        monitorInstancesResponseConv

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
    ec2QuerySource "DescribeSpotInstanceRequests" params $
        itemConduit "spotInstanceRequestSet" spotInstanceRequestConv
  where
    params =
        [ "SpotInstanceRequestId" |.#= requests
        , filtersParam filters
        ]

spotInstanceRequestConv :: (MonadThrow m, Applicative m)
    => SimpleXML -> m SpotInstanceRequest
spotInstanceRequestConv xml =
    SpotInstanceRequest
    <$> xml .< "spotInstanceRequestId"
    <*> xml .< "spotPrice"
    <*> xml .< "type"
    <*> xml .< "state"
    <*> getElementM xml "fault" (\xml' ->
        SpotInstanceFault
        <$> xml' .< "code"
        <*> xml' .< "message"
        )
    <*> getElement xml "status" (\xml' ->
        SpotInstanceStatus
        <$> xml' .< "code"
        <*> xml' .< "updateTime"
        <*> xml' .< "message"
        )
    <*> xml .< "validFrom"
    <*> xml .< "validUntil"
    <*> xml .< "launchGroup"
    <*> xml .< "availabilityZoneGroup"
    <*> spotInstanceLaunchSpecificationConv "launchSpecification" xml
    <*> xml .< "instanceId"
    <*> xml .< "createTime"
    <*> xml .< "productDescription"
    <*> resourceTagConv xml
    <*> xml .< "launchedAvailabilityZone"

spotInstanceLaunchSpecificationConv :: (MonadThrow m, Applicative m)
    => Text -> SimpleXML -> m SpotInstanceLaunchSpecification
spotInstanceLaunchSpecificationConv label xml = getElement xml label $ \xml' ->
    SpotInstanceLaunchSpecification
    <$> xml' .< "imageId"
    <*> xml' .< "keyName"
    <*> groupSetConv xml'
    <*> xml' .< "instanceType"
    <*> getElement xml' "placement" (\xml'' ->
        Placement
        <$> xml'' .< "availabilityZone"
        <*> xml'' .< "groupName"
        <*> xml'' .< "tenancy"
        )
    <*> xml' .< "kernelId"
    <*> xml' .< "ramdiskId"
    <*> spotInstanceBlockDeviceMappingsConv xml'
    <*> getElement xml' "monitoring" (\xml'' ->
        SpotInstanceMonitoringState
        <$> xml'' .< "enabled"
        )
    <*> xml' .< "subnetId"
    <*> spotInstanceNetworkInterfaceConv xml'
    <*> getElementM xml' "iamInstanceProfile" (\xml'' ->
        IamInstanceProfile
        <$> xml'' .< "arn"
        <*> xml'' .< "id"
        )
    <*> xml .< "ebsOptimized"

spotInstanceBlockDeviceMappingsConv :: (MonadThrow m, Applicative m)
    => SimpleXML -> m [SpotInstanceBlockDeviceMapping]
spotInstanceBlockDeviceMappingsConv xml = itemsSet xml "blockDeviceMapping" $ \xml' ->
    SpotInstanceBlockDeviceMapping
    <$> xml' .< "deviceName"
    <*> getElement xml' "ebs" (\xml'' ->
        EbsSpotInstanceBlockDevice
        <$> xml'' .< "volumeSize"
        <*> xml'' .< "deleteOnTermination"
        <*> xml'' .< "volumeType"
        )


spotInstanceNetworkInterfaceConv :: (MonadThrow m, Applicative m)
    => SimpleXML -> m [SpotInstanceNetworkInterface]
spotInstanceNetworkInterfaceConv xml = itemsSet xml "networkInterfaceSet" $ \xml' ->
    SpotInstanceNetworkInterface
    <$> xml' .< "deviceIndex"
    <*> xml' .< "subnetId"
    <*> securityGroupSetConv xml'

securityGroupSetConv :: (MonadThrow m, Applicative m)
    => SimpleXML -> m [SpotInstanceSecurityGroup]
securityGroupSetConv xml = itemsSet xml "groupSet" $ \xml' ->
    SpotInstanceSecurityGroup
    <$> xml' .< "groupId"

------------------------------------------------------------
-- RequestSpotInstances
------------------------------------------------------------
-- | 'RequestSpotInstancesParam' is genereted with 'defaultRequestSpotInstancesParam'
requestSpotInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => RequestSpotInstancesParam
    -> EC2 m [SpotInstanceRequest]
requestSpotInstances param =
    ec2Query "RequestSpotInstances" params $ xmlParser $ \xml ->
        itemsSet xml "spotInstanceRequestSet" spotInstanceRequestConv
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
    ec2QuerySource "CancelSpotInstanceRequests" params $
        itemConduit "spotInstanceRequestSet" cancelSpotInstanceResponseConv
  where
    params = ["SpotInstanceRequestId" |.#= requestIds]

cancelSpotInstanceResponseConv :: (MonadThrow m, Applicative m)
    => SimpleXML -> m CancelSpotInstanceRequestsResponse
cancelSpotInstanceResponseConv xml = CancelSpotInstanceRequestsResponse
    <$> xml .< "spotInstanceRequestId"
    <*> xml .< "state"
