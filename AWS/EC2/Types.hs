module AWS.EC2.Types
    ( Address(..)
    , AddressDomain(..)
    , AllocateAddress(..)
    , AMIAttribute(..)
    , AMIAttributeDescription(..)
    , Architecture(..)
    , AssociateAddressRequest(..)
    , Attachment(..)
    , AttachmentState(..)
    , AttachmentSetItemResponse(..)
    , AttachmentSetItemResponseStatus(..)
    , AvailabilityZone(..)
    , AvailabilityZoneMessage
    , BlockDeviceMapping(..)
    , BlockDeviceMappingParam(..)
    , ConsoleOutput(..)
    , CreateSubnetRequest(..)
    , CreateVolumeRequest(..)
    , CreateVpnGatewayType(..)
    , CustomerGateway(..)
    , CustomerGatewayState(..)
    , DisassociateAddressRequest(..)
    , EbsBlockDevice(..)
    , EbsInstanceBlockDevice(..)
    , EbsSource(..)
    , EC2Return(..)
    , Filter
    , Group(..)
    , Hypervisor(..)
    , IamInstanceProfile(..)
    , IcmpTypeCode(..)
    , Image(..)
    , ImageState(..)
    , ImageType(..)
    , Instance(..)
    , InstanceAttribute(..)
    , InstanceAttributeRequest(..)
    , InstanceBlockDeviceMapping(..)
    , InstanceLifecycle(..)
    , InstanceMonitoringState(..)
    , InstanceNetworkInterface(..)
    , InstanceNetworkInterfaceAssociation(..)
    , InstanceNetworkInterfaceAttachment(..)
    , InstancePrivateIpAddress(..)
    , InstanceState(..)
    , InstanceStateChange(..)
    , InstanceStatus(..)
    , InstanceStatusEvent(..)
    , InstanceStatusEventCode(..)
    , InstanceStatusType(..)
    , InstanceStatusTypeStatus(..)
    , InstanceStatusDetail(..)
    , InstanceStatusDetailName
    , InstanceStatusDetailStatus
    , IpPermission(..)
    , KeyPair(..)
    , ModifyInstanceAttributeRequest(..)
    , LaunchPermissionItem(..)
    , NetworkAcl(..)
    , NetworkAclAssociation(..)
    , NetworkAclEntry(..)
    , NetworkAclEntryRequest(..)
    , NetworkAclRuleAction(..)
    , NetworkInterface(..)
    , NetworkInterfaceAssociation(..)
    , NetworkInterfaceAttachment(..)
    , NetworkInterfaceParam(..)
    , NetworkInterfacePrivateIpAddress(..)
    , NetworkInterfaceStatus(..)
    , PasswordData(..)
    , Placement(..)
    , Platform(..)
    , PortRange(..)
    , ProductCode(..)
    , ProductCodeItem(..)
    , ProductCodeType(..)
    , Region(..)
    , RegisterImageRequest(..)
    , Reservation(..)
    , ResetInstanceAttributeRequest(..)
    , ResourceTag(..)
    , RootDeviceType(..)
    , Route(..)
    , RouteTable(..)
    , RouteTableAssociation(..)
    , RouteState(..)
    , RouteOrigin(..)
    , RunInstancesRequest(..)
    , SecondaryPrivateIpAddressParam(..)
    , SecurityGroup(..)
    , SecurityGroupRequest(..)
    , ShutdownBehavior(..)
    , Snapshot(..)
    , SnapshotStatus(..)
    , StateReason(..)
    , Subnet(..)
    , SubnetState(..)
    , Tag(..)
    , UserIdGroupPair(..)
    , VirtualizationType(..)
    , Volume(..)
    , VolumeAttribute(..)
    , VolumeAttributeRequest(..)
    , VolumeType(..)
    , VolumeState(..)
    , VolumeStatus(..)
    , VolumeStatusAction(..)
    , VolumeStatusEvent(..)
    , VolumeStatusDetail(..)
    , VolumeStatusInfo(..)
    , VolumeStatusInfoStatus(..)
    , Vpc(..)
    , VpcState(..)
    , VpnConnection(..)
    , VpnConnectionOptionsRequest(..)
    , VpnConnectionState(..)
    , VpnGateway(..)
    , VpnGatewayState(..)
    , VpnStaticRoute(..)
    , VpnStaticRouteSource(..)
    , VpnStaticRouteState(..)
    , VpnTunnelTelemetry(..)
    , VpnTunnelTelemetryStatus(..)
    , InternetGateway(..)
    , InternetGatewayAttachment(..)
    , InternetGatewayAttachmentState(..)
    ) where

import Data.IP (IPv4, AddrRange)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Time (UTCTime)

data Image = Image
    { imageId :: Text
    , imageLocation :: Text
    , imageImageState :: ImageState
    , imageOwnerId :: Text
    , imageIsPublic :: Bool
    , imageProductCodes :: [ProductCode]
    , imageArchitecture :: Text
    , imageImageType :: ImageType
    , imageKernelId :: Maybe Text
    , imageRamdiskId :: Maybe Text
    , imagePlatform :: Platform
    , imageStateReason :: Maybe StateReason
    , imageViridianEnabled :: Maybe Bool
    , imageOwnerAlias :: Maybe Text
    , imageName :: Text
    , imageDescription :: Text
    , imageBillingProducts :: [Text]
    , imageRootDeviceType :: RootDeviceType
    , imageRootDeviceName :: Maybe Text
    , imageBlockDeviceMappings :: [BlockDeviceMapping]
    , imageVirtualizationType :: VirtualizationType
    , imageTagSet :: [ResourceTag]
    , imageHypervisor :: Hypervisor
    }
  deriving (Show, Eq)

data AMIAttribute
    = AMIDescription
    | AMIKernel
    | AMIRamdisk
    | AMILaunchPermission
    | AMIProductCodes
    | AMIBlockDeviceMapping
  deriving (Show, Eq)

data AMIAttributeDescription = AMIAttributeDescription
    { amiAttributeDescriptionImageId :: Text
    , amiAttributeDescriptionLaunchPermission :: [LaunchPermissionItem]
    , amiAttributeDescriptionProductCodes :: [ProductCodeItem]
    , amiAttributeDescriptionKernel :: Maybe Text
    , amiAttributeDescriptionRamdisk :: Maybe Text
    , amiAttributeDescriptionDescription :: Maybe Text
    , amiAttributeDescriptionBlockDeviceMapping :: [BlockDeviceMapping]
    }
  deriving (Show, Eq)

data LaunchPermissionItem = LaunchPermissionItem
    { launchPermissionItemGroup :: Text
    , launchPermissionUserId :: Text
    }
  deriving (Show, Eq)

data ImageState
    = ImageStateAvailable
    | ImageStatePending
    | ImageStateFailed
  deriving (Show, Eq)

data ProductCode = ProductCode
    { productCodeCode :: Text
    , productCodeType :: ProductCodeType
    }
  deriving (Show, Eq)

data ProductCodeItem = ProductCodeItem
    { productCodeItemProductCode :: Text
    }
  deriving (Show, Eq)

data ProductCodeType
    = ProductCodeDevpay
    | ProductCodeMarketplace
  deriving (Show, Eq)

data ImageType
    = ImageTypeMachine
    | ImageTypeKernel
    | ImageTypeRamDisk
  deriving (Show, Eq)

data Platform
    = PlatformWindows
    | PlatformOther
  deriving (Show, Eq)

data StateReason = StateReason
    { stateReasonCode :: Text
    , stateReasonMessage :: Text
    }
  deriving (Show, Eq)

data RootDeviceType
    = RootDeviceTypeEBS
    | RootDeviceTypeInstanceStore
  deriving (Show, Eq)

data BlockDeviceMapping = BlockDeviceMapping
    { blockDeviceMappingDeviceName :: Text
    , blockDeviceMappingVirtualName :: Maybe Text
    , blockDeviceMappingEbs :: Maybe EbsBlockDevice
    }
  deriving (Show, Eq)

data EbsBlockDevice = EbsBlockDevice
    { ebsSnapshotId :: Maybe Text
    , ebsVolumeSize :: Int
    , ebsDeleteOnTermination :: Bool
    , ebsVolumeType :: VolumeType
    }
  deriving (Show, Eq)

data VolumeType
    = VolumeTypeStandard
    | VolumeTypeIO1 Int
  deriving (Show, Eq)

data VirtualizationType
    = VirtualizationTypeParavirtual
    | VirtualizationTypeHVM
  deriving (Show, Eq)

data ResourceTag = ResourceTag
    { resourceTagKey :: Text
    , resourceTagValue :: Maybe Text
    }
  deriving (Show, Eq)

data Hypervisor
    = HypervisorOVM
    | HypervisorXen
  deriving (Show, Eq)

data Region = Region
    { regionName :: Text
    , regionEndpoint :: Text
    }
  deriving (Show, Eq)

data AvailabilityZone = AvailabilityZone
    { zoneName :: Text
    , zoneState :: Text
    , zoneRegionName :: Text
    , zoneMessageSet :: [AvailabilityZoneMessage]
    }
  deriving (Show, Eq)

type AvailabilityZoneMessage = Text

data Reservation = Reservation
    { reservationId :: Text
    , reservationOwnerId :: Text
    , reservationGroupSet :: [Group]
    , reservationInstanceSet :: [Instance]
    , reservationRequesterId :: Maybe Text
    }
  deriving (Show, Eq)

data Instance = Instance
    { instanceId :: Text
    , instanceImageId :: Text
    , instanceState :: InstanceState
    , instancePrivateDnsName :: Text
    , instanceDnsName :: Text
    , instanceReason :: Text
    , instanceKeyName :: Maybe Text
    , instanceAmiLaunchIndex :: Text
    , instanceProductCodes :: [ProductCode]
    , instanceType :: Text
    , instanceLaunchTime :: UTCTime
    , instancePlacement :: Placement
    , instanceKernelId :: Maybe Text
    , instanceRamdiskId :: Maybe Text
    , instancePlatform :: Maybe Text
    , instanceMonitoring :: InstanceMonitoringState
    , instanceSubnetId :: Maybe Text
    , instanceVpcId :: Maybe Text
    , instancePrivateIpAddress :: Maybe IPv4
    , instanceIpAddress :: Maybe IPv4
    , instanceSourceDestCheck :: Maybe Bool
    , instancevpcGroupSet :: [Group]
    , instanceStateReason :: Maybe StateReason
    , instanceArchitecture :: Architecture
    , instanceRootDeviceType :: RootDeviceType
    , instanceRootDeviceName :: Maybe Text
    , instanceBlockDeviceMappings :: [InstanceBlockDeviceMapping]
    , instanceInstanceLifecycle :: InstanceLifecycle
    , instanceSpotInstanceRequestId :: Maybe Text
    , instanceVirtualizationType :: VirtualizationType
    , instanceClientToken :: Text
    , instanceTagSet :: [ResourceTag]
    , instanceHypervisor :: Hypervisor
    , instanceNetworkInterfaceSet :: [InstanceNetworkInterface]
    , instanceIamInstanceProfile :: Maybe IamInstanceProfile
    , instanceEbsOptimized :: Bool -- default: false
    }
  deriving (Show, Eq)

data InstanceStatus = InstanceStatus
    { instanceStatusInstanceId :: Text
    , instanceStatusAvailabilityZone :: Text
    , instanceStatusEventsSet :: [InstanceStatusEvent]
    , instanceStatusInstanceState :: InstanceState
    , instanceStatusSystemStatus :: InstanceStatusType
    , instanceStatusInstanceStatus :: InstanceStatusType
    }
  deriving (Show, Eq)

data InstanceStatusEvent = InstanceStatusEvent
    { instanceStatusEventCode :: InstanceStatusEventCode
    , instanceStatusEventDescription :: Text
    , instanceStatusEventNotBefore :: Maybe UTCTime
    , instanceStatusEventNotAfter :: Maybe UTCTime
    }
  deriving (Show, Eq)

data InstanceStatusEventCode
    = InstanceStatusEventCodeInstanceReboot
    | InstanceStatusEventCodeInstanceStop
    | InstanceStatusEventCodeSystemReboot
    | InstanceStatusEventCodeInstanceRetirement
  deriving (Show, Eq)

data InstanceStatusType = InstanceStatusType
    { instanceStatusTypeStatus :: InstanceStatusTypeStatus
    , instanceStatusTypeDetails :: [InstanceStatusDetail]
    }
  deriving (Show, Eq)

data InstanceStatusTypeStatus
    = InstanceStatusTypeStatusOK
    | InstanceStatusTypeStatusImpaired
    | InstanceStatusTypeStatusInsufficientData
    | InstanceStatusTypeStatusNotApplicable
  deriving (Show, Eq)

data InstanceStatusDetail = InstanceStatusDetail
    { instanceStatusDetailName :: InstanceStatusDetailName
    , instanceStatusDetailStatus :: InstanceStatusDetailStatus
    , instanceStatusDetailImpairedSince :: Maybe UTCTime
    }
  deriving (Show, Eq)

type InstanceStatusDetailName = Text

type InstanceStatusDetailStatus = Text

data Group = Group
    { groupId :: Text
    , groupName :: Text
    }
  deriving (Show, Eq)

data InstanceState
    = InstanceStatePending
    | InstanceStateRunning
    | InstanceStateShuttingDown
    | InstanceStateTerminated
    | InstanceStateStopping
    | InstanceStateStopped
    | InstanceStateUnknown Int
  deriving (Show, Eq)

data Placement = Placement
    { placementAvailabilityZone :: Text
    , placementGroupName :: Text
    , placementTenancy :: Text
    }
  deriving (Show, Eq)

data InstanceMonitoringState
    = MonitoringDisabled
    | MonitoringEnabled
    | MonitoringPending
  deriving (Show, Eq)

data Architecture = I386 | X86_64 deriving (Show, Eq)

data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping
    { instanceBlockDeviceMappingDeviceName :: Text
    , instanceBlockDeviceMappingEbs :: EbsInstanceBlockDevice
    }
  deriving (Show, Eq)

data EbsInstanceBlockDevice = EbsInstanceBlockDevice
    { ebsInstanceBlockDeviceVolumeId :: Text
    , ebsInstanceBlockDeviceState :: AttachmentSetItemResponseStatus
    , ebsInstanceBlockDeviceAttachTime :: UTCTime
    , ebsInstanceBlockDeviceDeleteOnTermination :: Bool
    }
  deriving (Show, Eq)

data InstanceLifecycle
    = LifecycleSpot
    | LifecycleNone
  deriving (Show, Eq)

data InstanceNetworkInterface = InstanceNetworkInterface
    { instanceNetworkInterfaceId :: Text
    , instanceNetworkInterfaceSubnetId :: Text
    , instanceNetworkInterfaceVpcId :: Text
    , instanceNetworkInterfaceDescription :: Text
    , instanceNetworkInterfaceOwnerId :: Text
    , instanceNetworkInterfaceStatus :: Text
    , instanceNetworkInterfacePrivateIpAddress :: IPv4
    , instanceNetworkInterfacePrivateDnsName :: Maybe Text
    , instanceNetworkInterfaceSourceDestCheck :: Bool
    , instanceNetworkInterfaceGroupSet :: [Group]
    , instanceNetworkInterfaceAttachment
        :: InstanceNetworkInterfaceAttachment
    , instanceNetworkInterfaceAssociation
        :: Maybe InstanceNetworkInterfaceAssociation
    , instanceNetworkInterfacePrivateIpAddressesSet
        :: [InstancePrivateIpAddress]
    }
  deriving (Show, Eq)

data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment
    { instanceNetworkInterfaceAttachmentId :: Text
    , instanceNetworkInterfaceAttachmentDeviceIndex :: Int
    , instanceNetworkInterfaceAttachmentStatus :: Text
    , instanceNetworkInterfaceAttachmentAttachTime :: UTCTime
    , instanceNetworkInterfaceAttachmentDeleteOnTermination
        :: Bool
    }
  deriving (Show, Eq)

data InstanceNetworkInterfaceAssociation
    = InstanceNetworkInterfaceAssociation
    { instanceNetworkInterfaceAssociationPublicIp :: Text
    , instanceNetworkInterfaceAssociationIpOwnerId :: Text
    }
  deriving (Show, Eq)

data InstancePrivateIpAddress = InstancePrivateIpAddress
    { instancePrivateIpAddressAddress :: IPv4
    , instancePrivateIpAddressPrimary :: Bool
    , instancePrivateIpAddressAssociation
        :: Maybe InstanceNetworkInterfaceAssociation
    }
  deriving (Show, Eq)

data IamInstanceProfile = IamInstanceProfile
    { iamInstanceProfileArn :: Text
    , iamInstanceProfileId :: Text
    }
  deriving (Show, Eq)

data ShutdownBehavior
    = ShutdownBehaviorStop
    | ShutdownBehaviorTerminate
  deriving (Show, Eq)

data InstanceAttribute
    = InstanceAttributeInstanceType Text
    | InstanceAttributeKernelId (Maybe Text)
    | InstanceAttributeRamdiskId (Maybe Text)
    | InstanceAttributeUserData (Maybe Text)
    | InstanceAttributeDisableApiTermination Bool
    | InstanceAttributeShutdownBehavior ShutdownBehavior
    | InstanceAttributeRootDeviceName (Maybe Text)
    | InstanceAttributeBlockDeviceMapping [InstanceBlockDeviceMapping]
    | InstanceAttributeSourceDestCheck (Maybe Bool)
    | InstanceAttributeGroupSet [Text]
    | InstanceAttributeProductCodes [ProductCode]
    | InstanceAttributeEbsOptimized Bool
  deriving (Show, Eq)

data Address = Address
    { addressPublicIp :: IPv4
    , addressAllocationId :: Maybe Text
    , addressDomain :: AddressDomain
    , addressInstanceId :: Maybe Text
    , addressAssociationId :: Maybe Text
    , addressNetworkInterfaceId :: Maybe Text
    , addressNetworkInterfaceOwnerId :: Maybe Text
    , addressPrivateIpAddress :: Maybe IPv4
    }
  deriving (Show, Eq)

data AddressDomain = AddressDomainStandard | AddressDomainVPC
  deriving (Show, Eq)

data AllocateAddress = AllocateAddress
    { allocateAddressPublicIp :: IPv4
    , allocateAddressDomain :: AddressDomain
    , allocateAddressAllocationId :: Maybe Text
    }
  deriving (Show, Eq)

data EC2Return = EC2Success | EC2Error Text
  deriving (Show, Eq)

data Tag = Tag
    { tagResourceId :: Text
    , tagResourceType :: Text
    , tagKey :: Text
    , tagValue :: Maybe Text
    }
  deriving (Show, Eq)

data InstanceStateChange = InstanceStateChange
    { instanceStateChangeInstanceId :: Text
    , instanceStateChangeCurrentState :: InstanceState
    , instanceStateChangePreviousState :: InstanceState
    }
  deriving (Show, Eq)

data ConsoleOutput = ConsoleOutput
    { consoleOutputInstanceId :: Text
    , consoleOutputTimestamp :: UTCTime
        -- ^ The time the data was last updated.
    , consoleOutputOutput :: Text
    }
  deriving (Show, Eq)

data PasswordData = PasswordData
    { passwordDataInstanceId :: Text
    , passwordDataTimestamp :: UTCTime
      -- ^ The time the data was last updated.
    , passwordDataPasswordData :: Text
    }
  deriving (Show, Eq)

data Snapshot = Snapshot
    { snapshotId :: Text
    , snapshotVolumeId :: Text
    , snapshotStatus :: SnapshotStatus
    , snapshotStartTime :: UTCTime
    , snapshotProgress :: Text
    , snapshotOwnerId :: Text
    , snapshotVolumeSize :: Int
    , snapshotDescription :: Text
    , snapshotOwnerAlias :: Maybe Text
    , snapshotTagSet :: [ResourceTag]
    }
  deriving (Show, Eq)

data SnapshotStatus
    = SnapshotPending
    | SnapshotCompleted
    | SnapshotError
  deriving (Show, Eq)

data Volume = Volume
    { volumeId :: Text
    , volumeSize :: Int
    , volumeSnapshotId :: Maybe Text
    , volumeAvailabilityZone :: Text
    , volumeStatus :: VolumeState
    , volumeCreateTime :: UTCTime
    , volumeAttachmentSet :: [AttachmentSetItemResponse]
    , volumeTagSet :: [ResourceTag]
    , volumeVolumeType :: VolumeType
    }
  deriving (Show, Eq)

data VolumeState
    = VolumeStateCreating
    | VolumeStateAvailable
    | VolumeStateInUse
    | VolumeStateDeleting
    | VolumeStateDeleted
    | VolumeStateError
  deriving (Show, Eq)

data AttachmentSetItemResponse = AttachmentSetItemResponse
    { attachmentSetItemResponseVolumeId :: Text
    , attachmentSetItemResponseInstanceId :: Text
    , attachmentSetItemResponseDevice :: Text
    , attachmentSetItemResponseStatus
        :: AttachmentSetItemResponseStatus
    , attachmentSetItemResponseAttachTime :: UTCTime
    , attachmentSetItemResponseDeleteOnTermination :: Maybe Bool
    }
  deriving (Show, Eq)

data AttachmentSetItemResponseStatus
    = AttachmentSetItemResponseStatusAttaching
    | AttachmentSetItemResponseStatusAttached
    | AttachmentSetItemResponseStatusDetaching
    | AttachmentSetItemResponseStatusDetached
  deriving (Show, Eq)

data KeyPair = KeyPair
    { keyPairName :: Text
    , keyPairFingerprint :: Text
    }
  deriving (Show, Eq)

data SecurityGroup = SecurityGroup
    { securityGroupOwnerId :: Text
    , securityGroupId :: Text
    , securityGroupName :: Text
    , securityGroupDescription :: Text
    , securityGroupVpcId :: Maybe Text
    , securityGroupIpPermissions :: [IpPermission]
    , securityGroupIpPermissionsEgress :: [IpPermission]
    , securityGroupTagSet :: [ResourceTag]
    }
  deriving (Show, Eq)

data IpPermission = IpPermission
    { ipPermissionIpProtocol :: Text
    , ipPermissionFromPort :: Maybe Int
    , ipPermissionToPort :: Maybe Int
    , ipPermissionGroups :: [UserIdGroupPair]
    , ipPermissionIpRanges :: [AddrRange IPv4]
    }
  deriving (Show, Eq)

data UserIdGroupPair = UserIdGroupPair
    { userIdGroupPairUserId :: Maybe Text
    , userIdGroupPairGroupId :: Text
    , userIdGroupPairGroupName :: Maybe Text
    }
  deriving (Show, Eq)

data BlockDeviceMappingParam
    = BlockDeviceMappingParamEbs
        { blockDeviceMappingParamEbsDeviceName :: Text
        , blockDeviceMappingParamEbsNoDevice :: Maybe Bool
        , blockDeviceMappingParamEbsSource :: EbsSource
        , blockDeviceMappingParamEbsDeleteOnTermination
            :: Maybe Bool
        , blockDeviceMappingParamEbsVolumeType :: Maybe VolumeType
        }
    | BlockDeviceMappingParamInstanceStore
        { blockDeviceMappingParamInstanceStoreDeviceName :: Text
        , blockDeviceMappingParamInstanceStoreNoDevice
            :: Maybe Bool
        , blockDeviceMappingParamInstanceStoreVirtualName
            :: Maybe Text
        }
  deriving (Show, Eq)

data EbsSource
    = EbsSourceSnapshotId Text
    | EbsSourceVolumeSize Int
  deriving (Show, Eq)

data NetworkInterfaceParam
    = NetworkInterfaceParamCreate
        { networkInterfaceParamCreateDeviceIndex :: Int
        , networkInterfaceParamCreateSubnetId :: Text
        , networkInterfaceParamCreateDescription :: Text
        , networkInterfaceParamCreatePrivateIpAddress
            :: Maybe IPv4
        , networkInterfaceParamCreatePrivateIpAddresses
            :: SecondaryPrivateIpAddressParam
        , networkInterfaceParamCreateSecurityGroupIds :: [Text]
        , networkInterfaceParamCreateDeleteOnTermination :: Bool
        }
    | NetworkInterfaceParamAttach
        { networkInterfaceParamAttachInterfaceId :: Text
        , networkInterfaceParamAttachDeviceIndex :: Int
        , networkInterfaceParamAttachDeleteOnTermination :: Bool
        }
  deriving (Show, Eq)

data SecondaryPrivateIpAddressParam
    = SecondaryPrivateIpAddressParamNothing
    | SecondaryPrivateIpAddressParamCount Int
    | SecondaryPrivateIpAddressParamSpecified
      { secondaryPrivateIpAddressParamSpecifiedAddresses :: [IPv4]
      , secondaryPrivateIpAddressParamSpecifiedPrimary
        :: Maybe Int
      }
  deriving (Show, Eq)

data VpnConnection = VpnConnection
    { vpnConnectionId :: Text
    , vpnConnectionState :: VpnConnectionState
    , vpnConnectionCustomerGatewayConfiguration :: Text
    , vpnConnectionType :: Text
    , vpnConnectionCustomerGatewayId :: Text
    , vpnConnectionVpnGatewayId :: Text
    , vpnConnectionTagSet :: [ResourceTag]
    , vpnConnectionVgwTelemetry :: [VpnTunnelTelemetry]
    , vpnConnectionOptions :: Maybe VpnConnectionOptionsRequest
    , vpnConnectionRoutes :: Maybe VpnStaticRoute
    }
  deriving (Show, Eq)

data VpnConnectionState
    = VpnConnectionStatePending
    | VpnConnectionStateAvailable
    | VpnConnectionStateDeleting
    | VpnConnectionStateDeleted
  deriving (Show, Eq)

data VpnTunnelTelemetry = VpnTunnelTelemetry
    { vpnTunnelTelemetryOutsideIpAddress :: IPv4
    , vpnTunnelTelemetryStatus :: VpnTunnelTelemetryStatus
    , vpnTunnelTelemetryLastStatusChange :: UTCTime
    , vpnTunnelTelemetryStatusMessage :: Text
    , vpnTunnelTelemetryAcceptRouteCount :: Int
    }
  deriving (Show, Eq)

data VpnTunnelTelemetryStatus
    = VpnTunnelTelemetryStatusUp
    | VpnTunnelTelemetryStatusDown
  deriving (Show, Eq)

data VpnConnectionOptionsRequest = VpnConnectionOptionsRequest
    { vpnConnectionOptionsRequestStaticRoutesOnly :: Bool
    }
  deriving (Show, Eq)

data VpnStaticRoute = VpnStaticRoute
    { vpnStaticRouteDestinationCidrBlock :: Text
    , vpnStaticRouteSource :: VpnStaticRouteSource
    , vpnStaticRouteState :: VpnStaticRouteState
    }
  deriving (Show, Eq)

data VpnStaticRouteSource = VpnStaticRouteSourceStatic
  deriving (Show, Eq)

data VpnStaticRouteState
    = VpnStaticRouteStatePending
    | VpnStaticRouteStateAvailable
    | VpnStaticRouteStateDeleting
    | VpnStaticRouteStateDeleted
  deriving (Show, Eq)

data RunInstancesRequest = RunInstancesRequest
    { runInstancesRequestImageId :: Text -- ^ Required
    , runInstancesRequestMinCount :: Int -- ^ Required
    , runInstancesRequestMaxCount :: Int -- ^ Required
    , runInstancesRequestKeyName :: Maybe Text
    , runInstancesRequestSecurityGroupIds :: [Text]
      -- ^ SecurityGroupIds (Required for VPC; optional for EC2)
    , runInstancesRequestSecurityGroups :: [Text]
      -- ^ SecurityGroups (Only for EC2; either id or name is accepted)
    , runInstancesRequestUserData :: Maybe ByteString
      -- ^ UserData (Base64-encoded MIME user data)
    , runInstancesRequestInstanceType :: Maybe Text
    , runInstancesRequestAvailabilityZone :: Maybe Text
    , runInstancesRequestPlacementGroup :: Maybe Text
    , runInstancesRequestTenancy :: Maybe Text
    , runInstancesRequestKernelId :: Maybe Text
    , runInstancesRequestRamdiskId :: Maybe Text
    , runInstancesRequestBlockDeviceMappings
        :: [BlockDeviceMappingParam]
    , runInstancesRequestMonitoringEnabled :: Maybe Bool
    , runInstancesRequestSubnetId :: Maybe Text
    , runInstancesRequestDisableApiTermination :: Maybe Bool
    , runInstancesRequestShutdownBehavior
        :: Maybe ShutdownBehavior
    , runInstancesRequestPrivateIpAddress :: Maybe IPv4
    , runInstancesRequestClientToken :: Maybe Text
    , runInstancesRequestNetworkInterfaces
        :: [NetworkInterfaceParam]
    , runInstancesRequestIamInstanceProfile
        :: Maybe IamInstanceProfile
    , runInstancesRequestEbsOptimized :: Maybe Bool
    }
  deriving (Show, Eq)

data InstanceAttributeRequest
    = InstanceAttributeRequestInstanceType
    | InstanceAttributeRequestKernelId
    | InstanceAttributeRequestRamdiskId
    | InstanceAttributeRequestUserData
    | InstanceAttributeRequestDisableApiTermination
    | InstanceAttributeRequestShutdownBehavior
    | InstanceAttributeRequestRootDeviceName
    | InstanceAttributeRequestBlockDeviceMapping
    | InstanceAttributeRequestSourceDestCheck
    | InstanceAttributeRequestGroupSet
    | InstanceAttributeRequestProductCodes
    | InstanceAttributeRequestEbsOptimized
  deriving (Show, Eq, Ord)

data ResetInstanceAttributeRequest
    = ResetInstanceAttributeRequestKernel
    | ResetInstanceAttributeRequestRamdisk
    | ResetInstanceAttributeRequestSourceDestCheck
  deriving (Show, Eq)

data ModifyInstanceAttributeRequest
    = ModifyInstanceAttributeRequestInstanceType Text
    | ModifyInstanceAttributeRequestKernelId Text
    | ModifyInstanceAttributeRequestRamdiskId Text
    | ModifyInstanceAttributeRequestUserData Text
    | ModifyInstanceAttributeRequestDisableApiTermination Bool
    | ModifyInstanceAttributeRequestShutdownBehavior
        ShutdownBehavior
    | ModifyInstanceAttributeRequestRootDeviceName Text
    | ModifyInstanceAttributeRequestBlockDeviceMapping
        [BlockDeviceMappingParam]
    | ModifyInstanceAttributeRequestSourceDestCheck Bool
    | ModifyInstanceAttributeRequestGroupSet [Text]
    | ModifyInstanceAttributeRequestEbsOptimized Bool
  deriving (Show, Eq)

data RegisterImageRequest = RegisterImageRequest
    { registerImageRequestName :: Text
    , registerImageRequestImageLocation :: Maybe Text
    , registerImageRequestDescription :: Maybe Text
    , registerImageRequestArchitecture :: Maybe Text
    , registerImageRequestKernelId :: Maybe Text
    , registerImageRequestRamdiskId :: Maybe Text
    , registerImageRequestRootDeviceName :: Maybe Text
    , registerImageRequestBlockDeviceMappings
        :: [BlockDeviceMappingParam]
    }
  deriving (Show, Eq)

data CreateVolumeRequest
    = CreateNewVolume
        { createNewVolumeSize :: Int
        , createNewVolumeAvailabilityZone :: Text
        , createNewVolumeVolumeType :: Maybe VolumeType
        }
    | CreateFromSnapshot
        { createFromSnapshotSnapshotId :: Text
        , createFromSnapshotAvailabilityZone :: Text
        , createFromSnapshotSize :: Maybe Int
        , createFromSnapshotVolumeType :: Maybe VolumeType
        }
  deriving (Show, Eq)

data AssociateAddressRequest
    = AssociateAddressRequestEc2
        { associateAddressRequestEc2PublicIp :: IPv4
        , associateAddressRequestEc2InstanceId :: Text
        }
    | AssociateAddressRequestVpc
        { associateAddressRequestVpcAllocationId :: Text
        , associateAddressRequestVpcInstanceId :: Maybe Text
        , associateAddressRequestVpcNetworkInterfaceId
            :: Maybe Text
        , associateAddressRequestVpcPrivateIpAddress :: Maybe IPv4
        , associateAddressRequestVpcAllowReassociation
            :: Maybe Bool
        }
  deriving (Show, Eq)

data DisassociateAddressRequest
    = DisassociateAddressRequestEc2 IPv4 -- ^ PublicIp for EC2
    | DisassociateAddressRequestVpc IPv4
      -- ^ AssociationId for VPC
  deriving (Show, Eq)

data SecurityGroupRequest
    = SecurityGroupRequestGroupId Text
    | SecurityGroupRequestGroupName Text
  deriving (Show, Eq)

data Subnet = Subnet
    { subnetId :: Text
    , subnetState :: SubnetState
    , subnetVpicId :: Text
    , subnetCidrBlock :: AddrRange IPv4
    , subnetAvailableIpAddressCount :: Int
    , subnetAvailabilityZone :: Text
    , subnetTagSet :: [ResourceTag]
    }
  deriving (Show, Eq)

data SubnetState = SubnetStatePending | SubnetStateAvailable
  deriving (Show, Eq)

data CreateSubnetRequest = CreateSubnetRequest
    { createSubnetRequestVpcId :: Text
    , createSubnetRequestCidrBlock :: AddrRange IPv4
    , createSubnetRequestAvailabilityZone :: Maybe Text
    }
  deriving (Show, Eq)

data VolumeStatus = VolumeStatus
    { volumeStatusVolumeId :: Text
    , volumeStatusAvailabilityZone :: Text
    , volumeStatusVolumeStatus :: VolumeStatusInfo
    , volumeStatusEventSet :: [VolumeStatusEvent]
    , volumeStatusActionSet :: [VolumeStatusAction]
    }
  deriving (Show, Eq)

data VolumeStatusInfo = VolumeStatusInfo
    { volumeStatusInfoStatus :: VolumeStatusInfoStatus
    , volumeStatusInfoDetails :: [VolumeStatusDetail]
    }
  deriving (Show, Eq)

data VolumeStatusInfoStatus
    = VolumeStatusInfoStatusOK
    | VolumeStatusInfoStatusImpaired
    | VolumeStatusInfoStatusInsufficientData
  deriving (Show, Eq)

data VolumeStatusDetail = VolumeStatusDetail
    { volumeStatusDetailName :: Text
    , volumeStatusDetailStatus :: Text
    }
  deriving (Show, Eq)

data VolumeStatusEvent = VolumeStatusEvent
    { volumeStatusEventType :: Text
    , volumeStatusEventId :: Text
    , volumeStatusEventDescription :: Text
    , volumeStatusEventNotBefore :: Maybe UTCTime
    , volumeStatusEventNotAfter :: Maybe UTCTime
    }
  deriving (Show, Eq)

data VolumeStatusAction = VolumeStatusAction
    { volumeStatusActionCode :: Text
    , volumeStatusActionEventType :: Text
    , volumeStatusActionEventId :: Text
    , volumeStatusActionDescription :: Text
    }
  deriving (Show, Eq)

data VolumeAttribute
    = VolumeAttributeAutoEnableIO Bool
    | VolumeAttributeProductCodes [ProductCode]
  deriving (Show, Eq)

data VolumeAttributeRequest
    = VolumeAttributeRequestAutoEnableIO
    | VolumeAttributeRequestProductCodes
  deriving (Show, Eq)

data NetworkAcl = NetworkAcl
    { networkAclId :: Text
    , networkAclVpcId :: Text
    , networkAclDefault :: Bool
    , networkAclEntrySet :: [NetworkAclEntry]
    , networkAclAssociationSet :: [NetworkAclAssociation]
    , networkAclTagSet :: [ResourceTag]
    }
  deriving (Show, Eq)

data NetworkAclEntry = NetworkAclEntry
    { networkAclEntryRuleNumber :: Int
    , networkAclEntryProtocol :: Int
    , networkAclEntryRuleAction :: NetworkAclRuleAction
    , networkAclEntryEgress :: Bool
    , networkAclEntryCidrBlock :: Text
    , networkAclEntryIcmpTypeCode :: Maybe IcmpTypeCode
    , networkAclEntryPortRange :: Maybe PortRange
    }
  deriving (Show, Eq)

data NetworkAclRuleAction
    = NetworkAclRuleActionAllow
    | NetworkAclRuleActionDeny
  deriving (Show, Eq)

data IcmpTypeCode = IcmpTypeCode
    { icmpTypeCodeCode :: Int
    , icmpTypeCodeType :: Int
    }
  deriving (Show, Eq)

data PortRange = PortRange
    { portRangeFrom :: Int
    , portRangeTo :: Int
    }
  deriving (Show, Eq)

data NetworkAclAssociation = NetworkAclAssociation
    { networkAclAssociationId :: Text
    , networkAclAssociationNetworkAclId :: Text
    , networkAclAssociationSubnetId :: Text
    }
  deriving (Show, Eq)

data NetworkAclEntryRequest = NetworkAclEntryRequest
    { networkAclEntryRequestNetworkAclId :: Text
    , networkAclEntryRequestRuleNumber :: Int
    , networkAclEntryRequestProtocol :: Int
      -- ^ Protocol Number <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xml>
    , networkAclEntryRequestRuleAction :: NetworkAclRuleAction
    , networkAclEntryRequestEgress :: Bool
    , networkAclEntryRequestCidrBlock :: Text
    , networkAclEntryRequestIcmp :: Maybe IcmpTypeCode
    , networkAclEntryRequestPortRange :: Maybe PortRange
    }
  deriving (Show, Eq)

data RouteTable = RouteTable
    { routeTableId :: Text
    , routeTableVpcId :: Text
    , routeTableRouteSet :: [Route]
    , routeTableAssociationSet :: [RouteTableAssociation]
    , routeTablePropagatingVgw :: Maybe PropagatingVgw
    , routeTableTagSet :: [ResourceTag]
    }
  deriving (Show, Eq)

data Route = Route
    { routeDestinationCidrBlock :: Text
    , routeGatewayId :: Maybe Text
    , routeInstanceId :: Maybe Text
    , routeInstanceOwnerId :: Maybe Text
    , routeNetworkInterfaceId :: Maybe Text
    , routeState :: RouteState
    , routeOrigin :: Maybe RouteOrigin
    }
  deriving (Show, Eq)

data RouteState = RouteStateActive | RouteStateBlackhole
  deriving (Show, Eq)

data RouteOrigin
    = RouteOriginCreateRouteTable
    | RouteOriginCreateRoute
    | RouteOriginTableEnableVgwRoutePropagation
  deriving (Show, Eq)

data RouteTableAssociation = RouteTableAssociation
    { routeTableAssociationId :: Text
    , routeTableAssociationRouteTableId :: Text
    , routeTableAssociationSubnetId :: Maybe Text
    , routeTableAssociationMain :: Maybe Bool
    }
  deriving (Show, Eq)

type PropagatingVgw = Text

data Vpc = Vpc
    { vpcId :: Text
    , vpcState :: VpcState
    , vpcCidrBlock :: AddrRange IPv4
    , vpcDhcpOptionsId :: Text
    , vpcTagSet :: [ResourceTag]
    , vpcInstanceTenancy :: Text
    }
  deriving (Show, Eq)

data VpcState = VpcStatePending | VpcStateAvailable
  deriving (Show, Eq)

data VpnGateway = VpnGateway
    { vpnGatewayId :: Text
    , vpnGatewayState :: VpnGatewayState
    , vpnGatewayType :: Text
    , vpnGatewayAvailabilityZone :: Maybe Text
    , vpnGatewayAttachments :: [Attachment]
    , vpnGatewayTagSet :: [ResourceTag]
    }
  deriving (Show, Eq)

data VpnGatewayState
    = VpnGatewayStatePending
    | VpnGatewayStateAvailable
    | VpnGatewayStateDeleting
    | VpnGatewayStateDeleted
  deriving (Show, Eq)

data Attachment = Attachment
    { attachmentVpcId :: Text
    , attachmentState :: AttachmentState
    }
  deriving (Show, Eq)

data AttachmentState
    = AttachmentStateAttaching
    | AttachmentStateAttached
    | AttachmentStateDetaching
    | AttachmentStateDetached
  deriving (Show, Eq)

data CreateVpnGatewayType = CreateVpnGatewayTypeIpsec1

data CustomerGateway = CustomerGateway
    { customerGatewayId :: Text
    , customerGatewayState :: CustomerGatewayState
    , customerGatewayType :: Text
    , customerGatewayIpAddress :: IPv4
    , customerGatewayBgpAsn :: Int
    , customerGatewayTagSet :: [ResourceTag]
    }
  deriving (Show, Eq)

data CustomerGatewayState
    = CustomerGatewayStatePending
    | CustomerGatewayStateAvailable
    | CustomerGatewayStateDeleting
    | CustomerGatewayStateDeleted
  deriving (Show, Eq)

data InternetGateway = InternetGateway
    { internetGatewayInternetGatewayId :: Text
    , internetGatewayAttachmentSet :: [InternetGatewayAttachment]
    , internetGatewayTagSet :: [ResourceTag]
    }
  deriving (Show, Eq)

data InternetGatewayAttachment = InternetGatewayAttachment
    { internetGatewayAttachmentVpcId :: Text
    , internetGatewayAttachmentState :: InternetGatewayAttachmentState
    }
  deriving (Show, Eq)

data InternetGatewayAttachmentState
    = InternetGatewayAttachmentStateAttaching
    | InternetGatewayAttachmentStateAttached
    | InternetGatewayAttachmentStateDetaching
    | InternetGatewayAttachmentStateDetached
    | InternetGatewayAttachmentStateAvailable
  deriving (Show, Eq)

data NetworkInterface = NetworkInterface
    { networkInterfaceId :: Text
    , networkInterfaceSubnetId :: Text
    , networkInterfaceVpcId :: Text
    , networkInterfaceAvailabilityZone :: Text
    , networkInterfaceDescription :: Text
    , networkInterfaceOwnerId :: Text
    , networkInterfaceRequesterId :: Maybe Text
    , networkInterfaceRequesterManaged :: Text
    , networkInterfaceStatus :: NetworkInterfaceStatus
    , networkInterfaceMacAddress :: Text
    , networkInterfacePrivateIpAddress :: IPv4
    , networkInterfacePrivateDnsName :: Maybe Text
    , networkInterfaceSourceDestCheck :: Bool
    , networkInterfaceGroupSet :: [Group]
    , networkInterfaceAttachment
        :: Maybe NetworkInterfaceAttachment
    , networkInterfaceAssociation
        :: Maybe NetworkInterfaceAssociation
    , networkInterfaceTagSet :: [ResourceTag]
    , networkInterfacePrivateIpAddressesSet
        :: [NetworkInterfacePrivateIpAddress]
    }
  deriving (Show, Eq)

type Filter = (Text, [Text])

data NetworkInterfaceStatus
    = NetworkInterfaceStatusAvailable
    | NetworkInterfaceStatusInUse
  deriving (Show, Eq)

data NetworkInterfaceAttachment = NetworkInterfaceAttachment
    { networkInterfaceAttachmentId :: Text
    , networkInterfaceAttachmentInstanceId :: Maybe Text
    , networkInterfaceAttachmentInstanceOwnerId :: Text
    , networkInterfaceAttachmentDeviceIndex :: Int
    , networkInterfaceAttachmentStatus :: Text
    , networkInterfaceAttachmentAttachTime :: UTCTime
    , networkInterfaceAttachmentDeleteOnTermination :: Bool
    }
  deriving (Show, Eq)

data NetworkInterfaceAssociation = NetworkInterfaceAssociation
    { networkInterfaceAssociationAttachmentId :: Maybe Text
    , networkInterfaceAssociationInstanceId :: Maybe Text
    , networkInterfaceAssociationPublicIp :: IPv4
    , networkInterfaceAssociationIpOwnerId :: Text
    , networkInterfaceAssociationId :: Text
    }
  deriving (Show, Eq)

data NetworkInterfacePrivateIpAddress
    = NetworkInterfacePrivateIpAddress
    { networkInterfacePrivateIpAddressPrivateIpAddress :: IPv4
    , networkInterfacePrivateIpAddressPrimary :: Bool
    , networkInterfacePrivateIpAddressAssociation
        :: Maybe NetworkInterfaceAssociation
    }
  deriving (Show, Eq)
