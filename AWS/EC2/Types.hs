{-# LANGUAGE TemplateHaskell #-}
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
    , CreateRouteRequest(..)
    , CreateSubnetRequest(..)
    , CreateVolumePermission(..)
    , CreateVolumePermissionItem(..)
    , CreateVolumeRequest(..)
    , CreateVpnGatewayType(..)
    , CustomerGateway(..)
    , CustomerGatewayState(..)
    , DhcpConfiguration(..)
    , DhcpOptions(..)
    , DhcpValue(..)
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
    , LaunchPermission(..)
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
    , ResetSnapshotAttributeRequest(..)
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
    , SnapshotAttribute(..)
    , SnapshotAttributeRequest(..)
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

import AWS.Class
import AWS.Lib.FromText

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
    , imageName :: Maybe Text
    , imageDescription :: Maybe Text
    , imageBillingProducts :: [Text]
    , imageRootDeviceType :: RootDeviceType
    , imageRootDeviceName :: Maybe Text
    , imageBlockDeviceMappings :: [BlockDeviceMapping]
    , imageVirtualizationType :: VirtualizationType
    , imageTagSet :: [ResourceTag]
    , imageHypervisor :: Hypervisor
    }
  deriving (Show, Read, Eq)

data AMIAttribute
    = AMIDescription
    | AMIKernel
    | AMIRamdisk
    | AMILaunchPermission
    | AMIProductCodes
    | AMIBlockDeviceMapping
  deriving (Show, Read, Eq)

data AMIAttributeDescription = AMIAttributeDescription
    { amiAttributeDescriptionImageId :: Text
    , amiAttributeDescriptionLaunchPermission :: [LaunchPermissionItem]
    , amiAttributeDescriptionProductCodes :: [ProductCodeItem]
    , amiAttributeDescriptionKernel :: Maybe Text
    , amiAttributeDescriptionRamdisk :: Maybe Text
    , amiAttributeDescriptionDescription :: Maybe Text
    , amiAttributeDescriptionBlockDeviceMapping :: [BlockDeviceMapping]
    }
  deriving (Show, Read, Eq)

data LaunchPermission = LaunchPermission
    { launchPermissionAdd :: [LaunchPermissionItem]
    , launchPermissionRemove :: [LaunchPermissionItem]
    }
  deriving (Show, Read, Eq)

data LaunchPermissionItem = LaunchPermissionItem
    { launchPermissionItemGroup :: Text
    , launchPermissionUserId :: Text
    }
  deriving (Show, Read, Eq)

data ImageState
    = ImageStateAvailable
    | ImageStatePending
    | ImageStateFailed
  deriving (Show, Read, Eq)

data ProductCode = ProductCode
    { productCodeCode :: Text
    , productCodeType :: ProductCodeType
    }
  deriving (Show, Read, Eq)

data ProductCodeItem = ProductCodeItem
    { productCodeItemProductCode :: Text
    }
  deriving (Show, Read, Eq)

data ProductCodeType
    = ProductCodeDevpay
    | ProductCodeMarketplace
  deriving (Show, Read, Eq)

data ImageType
    = ImageTypeMachine
    | ImageTypeKernel
    | ImageTypeRamDisk
  deriving (Show, Read, Eq)

data Platform
    = PlatformWindows
    | PlatformOther
  deriving (Show, Read, Eq)

data StateReason = StateReason
    { stateReasonCode :: Text
    , stateReasonMessage :: Text
    }
  deriving (Show, Read, Eq)

data RootDeviceType
    = RootDeviceTypeEBS
    | RootDeviceTypeInstanceStore
  deriving (Show, Read, Eq)

data BlockDeviceMapping = BlockDeviceMapping
    { blockDeviceMappingDeviceName :: Text
    , blockDeviceMappingVirtualName :: Maybe Text
    , blockDeviceMappingEbs :: Maybe EbsBlockDevice
    }
  deriving (Show, Read, Eq)

data EbsBlockDevice = EbsBlockDevice
    { ebsSnapshotId :: Maybe Text
    , ebsVolumeSize :: Int
    , ebsDeleteOnTermination :: Bool
    , ebsVolumeType :: VolumeType
    }
  deriving (Show, Read, Eq)

data VolumeType
    = VolumeTypeStandard
    | VolumeTypeIO1 Int
  deriving (Show, Read, Eq)

data VirtualizationType
    = VirtualizationTypeParavirtual
    | VirtualizationTypeHVM
  deriving (Show, Read, Eq)

data ResourceTag = ResourceTag
    { resourceTagKey :: Text
    , resourceTagValue :: Maybe Text
    }
  deriving (Show, Read, Eq)

data Hypervisor
    = HypervisorOVM
    | HypervisorXen
  deriving (Show, Read, Eq)

data Region = Region
    { regionName :: Text
    , regionEndpoint :: Text
    }
  deriving (Show, Read, Eq)

data AvailabilityZone = AvailabilityZone
    { zoneName :: Text
    , zoneState :: Text
    , zoneRegionName :: Text
    , zoneMessageSet :: [AvailabilityZoneMessage]
    }
  deriving (Show, Read, Eq)

type AvailabilityZoneMessage = Text

data Reservation = Reservation
    { reservationId :: Text
    , reservationOwnerId :: Text
    , reservationGroupSet :: [Group]
    , reservationInstanceSet :: [Instance]
    , reservationRequesterId :: Maybe Text
    }
  deriving (Show, Read, Eq)

data Instance = Instance
    { instanceId :: Text
    , instanceImageId :: Text
    , instanceState :: InstanceState
    , instancePrivateDnsName :: Maybe Text
    , instanceDnsName :: Maybe Text
    , instanceReason :: Maybe Text
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
    , instanceInstanceLifecycle :: Maybe InstanceLifecycle
    , instanceSpotInstanceRequestId :: Maybe Text
    , instanceVirtualizationType :: VirtualizationType
    , instanceClientToken :: Maybe Text
    , instanceTagSet :: [ResourceTag]
    , instanceHypervisor :: Hypervisor
    , instanceNetworkInterfaceSet :: [InstanceNetworkInterface]
    , instanceIamInstanceProfile :: Maybe IamInstanceProfile
    , instanceEbsOptimized :: Bool -- default: false
    }
  deriving (Show, Read, Eq)

data InstanceStatus = InstanceStatus
    { instanceStatusInstanceId :: Text
    , instanceStatusAvailabilityZone :: Text
    , instanceStatusEventsSet :: [InstanceStatusEvent]
    , instanceStatusInstanceState :: InstanceState
    , instanceStatusSystemStatus :: InstanceStatusType
    , instanceStatusInstanceStatus :: InstanceStatusType
    }
  deriving (Show, Read, Eq)

data InstanceStatusEvent = InstanceStatusEvent
    { instanceStatusEventCode :: InstanceStatusEventCode
    , instanceStatusEventDescription :: Text
    , instanceStatusEventNotBefore :: Maybe UTCTime
    , instanceStatusEventNotAfter :: Maybe UTCTime
    }
  deriving (Show, Read, Eq)

data InstanceStatusEventCode
    = InstanceStatusEventCodeInstanceReboot
    | InstanceStatusEventCodeInstanceStop
    | InstanceStatusEventCodeSystemReboot
    | InstanceStatusEventCodeInstanceRetirement
  deriving (Show, Read, Eq)

data InstanceStatusType = InstanceStatusType
    { instanceStatusTypeStatus :: InstanceStatusTypeStatus
    , instanceStatusTypeDetails :: [InstanceStatusDetail]
    }
  deriving (Show, Read, Eq)

data InstanceStatusTypeStatus
    = InstanceStatusTypeStatusOK
    | InstanceStatusTypeStatusImpaired
    | InstanceStatusTypeStatusInsufficientData
    | InstanceStatusTypeStatusNotApplicable
  deriving (Show, Read, Eq)

data InstanceStatusDetail = InstanceStatusDetail
    { instanceStatusDetailName :: InstanceStatusDetailName
    , instanceStatusDetailStatus :: InstanceStatusDetailStatus
    , instanceStatusDetailImpairedSince :: Maybe UTCTime
    }
  deriving (Show, Read, Eq)

type InstanceStatusDetailName = Text

type InstanceStatusDetailStatus = Text

data Group = Group
    { groupId :: Text
    , groupName :: Text
    }
  deriving (Show, Read, Eq)

data InstanceState
    = InstanceStatePending
    | InstanceStateRunning
    | InstanceStateShuttingDown
    | InstanceStateTerminated
    | InstanceStateStopping
    | InstanceStateStopped
    | InstanceStateUnknown Int
  deriving (Show, Read, Eq)

data Placement = Placement
    { placementAvailabilityZone :: Text
    , placementGroupName :: Maybe Text
    , placementTenancy :: Text
    }
  deriving (Show, Read, Eq)

data InstanceMonitoringState
    = MonitoringDisabled
    | MonitoringEnabled
    | MonitoringPending
  deriving (Show, Read, Eq)

data Architecture = I386 | X86_64 deriving (Show, Read, Eq)

data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping
    { instanceBlockDeviceMappingDeviceName :: Text
    , instanceBlockDeviceMappingEbs :: EbsInstanceBlockDevice
    }
  deriving (Show, Read, Eq)

data EbsInstanceBlockDevice = EbsInstanceBlockDevice
    { ebsInstanceBlockDeviceVolumeId :: Text
    , ebsInstanceBlockDeviceState :: AttachmentSetItemResponseStatus
    , ebsInstanceBlockDeviceAttachTime :: UTCTime
    , ebsInstanceBlockDeviceDeleteOnTermination :: Bool
    }
  deriving (Show, Read, Eq)

data InstanceLifecycle
    = LifecycleSpot
    | LifecycleNone
  deriving (Show, Read, Eq)

data InstanceNetworkInterface = InstanceNetworkInterface
    { instanceNetworkInterfaceId :: Text
    , instanceNetworkInterfaceSubnetId :: Maybe Text
    , instanceNetworkInterfaceVpcId :: Maybe Text
    , instanceNetworkInterfaceDescription :: Maybe Text
    , instanceNetworkInterfaceOwnerId :: Text
    , instanceNetworkInterfaceStatus :: Text
    , instanceNetworkInterfaceMacAddress :: Maybe Text
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
  deriving (Show, Read, Eq)

data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment
    { instanceNetworkInterfaceAttachmentId :: Text
    , instanceNetworkInterfaceAttachmentDeviceIndex :: Int
    , instanceNetworkInterfaceAttachmentStatus :: Text
    , instanceNetworkInterfaceAttachmentAttachTime :: UTCTime
    , instanceNetworkInterfaceAttachmentDeleteOnTermination
        :: Bool
    }
  deriving (Show, Read, Eq)

data InstanceNetworkInterfaceAssociation
    = InstanceNetworkInterfaceAssociation
    { instanceNetworkInterfaceAssociationPublicIp :: Text
    , instanceNetworkInterfaceAssociationIpOwnerId :: Text
    }
  deriving (Show, Read, Eq)

data InstancePrivateIpAddress = InstancePrivateIpAddress
    { instancePrivateIpAddressAddress :: IPv4
    , instancePrivateIpAddressDnsName :: Maybe Text
    , instancePrivateIpAddressPrimary :: Bool
    , instancePrivateIpAddressAssociation
        :: Maybe InstanceNetworkInterfaceAssociation
    }
  deriving (Show, Read, Eq)

data IamInstanceProfile = IamInstanceProfile
    { iamInstanceProfileArn :: Text
    , iamInstanceProfileId :: Text
    }
  deriving (Show, Read, Eq)

data ShutdownBehavior
    = ShutdownBehaviorStop
    | ShutdownBehaviorTerminate
  deriving (Show, Read, Eq)

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
  deriving (Show, Read, Eq)

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
  deriving (Show, Read, Eq)

data AddressDomain = AddressDomainStandard | AddressDomainVPC
  deriving (Show, Read, Eq)

data AllocateAddress = AllocateAddress
    { allocateAddressPublicIp :: IPv4
    , allocateAddressDomain :: AddressDomain
    , allocateAddressAllocationId :: Maybe Text
    }
  deriving (Show, Read, Eq)

data EC2Return = EC2Success | EC2Error Text
  deriving (Show, Read, Eq)

data Tag = Tag
    { tagResourceId :: Text
    , tagResourceType :: Text
    , tagKey :: Text
    , tagValue :: Maybe Text
    }
  deriving (Show, Read, Eq)

data InstanceStateChange = InstanceStateChange
    { instanceStateChangeInstanceId :: Text
    , instanceStateChangeCurrentState :: InstanceState
    , instanceStateChangePreviousState :: InstanceState
    }
  deriving (Show, Read, Eq)

data ConsoleOutput = ConsoleOutput
    { consoleOutputInstanceId :: Text
    , consoleOutputTimestamp :: UTCTime
        -- ^ The time the data was last updated.
    , consoleOutputOutput :: Text
    }
  deriving (Show, Read, Eq)

data PasswordData = PasswordData
    { passwordDataInstanceId :: Text
    , passwordDataTimestamp :: UTCTime
      -- ^ The time the data was last updated.
    , passwordDataPasswordData :: Text
    }
  deriving (Show, Read, Eq)

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
  deriving (Show, Read, Eq)

data SnapshotStatus
    = SnapshotPending
    | SnapshotCompleted
    | SnapshotError
  deriving (Show, Read, Eq)

data SnapshotAttributeRequest
    = SnapshotAttributeRequestCreateVolumePermission
    | SnapshotAttributeRequestProductCodes
  deriving (Show, Read, Eq)

data ResetSnapshotAttributeRequest
    = ResetSnapshotAttributeRequestCreateVolumePermission
  deriving (Show, Read, Eq)

data SnapshotAttribute = SnapshotAttribute
    { snapshotAttributeSnapshotId :: Text
    , snapshotAttributeCreateVolumePermissionItems
        :: [CreateVolumePermissionItem]
    , snapshotAttributeProductCodes :: [ProductCode]
    }
  deriving (Show, Read, Eq)

data CreateVolumePermission = CreateVolumePermission
    { createVolumePermissionAdd :: [CreateVolumePermissionItem]
    , createVolumePermissionRemove :: [CreateVolumePermissionItem]
    }
  deriving (Show, Read, Eq)

data CreateVolumePermissionItem = CreateVolumePermissionItem
    { createVolumePermissionItemUserId :: Maybe Text
    , createVolumePermissionItemGroup :: Maybe Text
    }
  deriving (Show, Read, Eq)

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
  deriving (Show, Read, Eq)

data VolumeState
    = VolumeStateCreating
    | VolumeStateAvailable
    | VolumeStateInUse
    | VolumeStateDeleting
    | VolumeStateDeleted
    | VolumeStateError
  deriving (Show, Read, Eq)

data AttachmentSetItemResponse = AttachmentSetItemResponse
    { attachmentSetItemResponseVolumeId :: Text
    , attachmentSetItemResponseInstanceId :: Text
    , attachmentSetItemResponseDevice :: Text
    , attachmentSetItemResponseStatus
        :: AttachmentSetItemResponseStatus
    , attachmentSetItemResponseAttachTime :: UTCTime
    , attachmentSetItemResponseDeleteOnTermination :: Maybe Bool
    }
  deriving (Show, Read, Eq)

data AttachmentSetItemResponseStatus
    = AttachmentSetItemResponseStatusAttaching
    | AttachmentSetItemResponseStatusAttached
    | AttachmentSetItemResponseStatusDetaching
    | AttachmentSetItemResponseStatusDetached
  deriving (Show, Read, Eq)

data KeyPair = KeyPair
    { keyPairName :: Text
    , keyPairFingerprint :: Text
    }
  deriving (Show, Read, Eq)

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
  deriving (Show, Read, Eq)

data IpPermission = IpPermission
    { ipPermissionIpProtocol :: Text
    , ipPermissionFromPort :: Maybe Int
    , ipPermissionToPort :: Maybe Int
    , ipPermissionGroups :: [UserIdGroupPair]
    , ipPermissionIpRanges :: [AddrRange IPv4]
    }
  deriving (Show, Read, Eq)

data UserIdGroupPair = UserIdGroupPair
    { userIdGroupPairUserId :: Maybe Text
    , userIdGroupPairGroupId :: Text
    , userIdGroupPairGroupName :: Maybe Text
    }
  deriving (Show, Read, Eq)

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
  deriving (Show, Read, Eq)

data EbsSource
    = EbsSourceSnapshotId Text
    | EbsSourceVolumeSize Int
  deriving (Show, Read, Eq)

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
  deriving (Show, Read, Eq)

data SecondaryPrivateIpAddressParam
    = SecondaryPrivateIpAddressParamNothing
    | SecondaryPrivateIpAddressParamCount Int
    | SecondaryPrivateIpAddressParamSpecified
      { secondaryPrivateIpAddressParamSpecifiedAddresses :: [IPv4]
      , secondaryPrivateIpAddressParamSpecifiedPrimary
        :: Maybe Int
      }
  deriving (Show, Read, Eq)

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
  deriving (Show, Read, Eq)

data VpnConnectionState
    = VpnConnectionStatePending
    | VpnConnectionStateAvailable
    | VpnConnectionStateDeleting
    | VpnConnectionStateDeleted
  deriving (Show, Read, Eq)

data VpnTunnelTelemetry = VpnTunnelTelemetry
    { vpnTunnelTelemetryOutsideIpAddress :: IPv4
    , vpnTunnelTelemetryStatus :: VpnTunnelTelemetryStatus
    , vpnTunnelTelemetryLastStatusChange :: UTCTime
    , vpnTunnelTelemetryStatusMessage :: Text
    , vpnTunnelTelemetryAcceptRouteCount :: Int
    }
  deriving (Show, Read, Eq)

data VpnTunnelTelemetryStatus
    = VpnTunnelTelemetryStatusUp
    | VpnTunnelTelemetryStatusDown
  deriving (Show, Read, Eq)

data VpnConnectionOptionsRequest = VpnConnectionOptionsRequest
    { vpnConnectionOptionsRequestStaticRoutesOnly :: Bool
    }
  deriving (Show, Read, Eq)

data VpnStaticRoute = VpnStaticRoute
    { vpnStaticRouteDestinationCidrBlock :: Text
    , vpnStaticRouteSource :: VpnStaticRouteSource
    , vpnStaticRouteState :: VpnStaticRouteState
    }
  deriving (Show, Read, Eq)

data VpnStaticRouteSource = VpnStaticRouteSourceStatic
  deriving (Show, Read, Eq)

data VpnStaticRouteState
    = VpnStaticRouteStatePending
    | VpnStaticRouteStateAvailable
    | VpnStaticRouteStateDeleting
    | VpnStaticRouteStateDeleted
  deriving (Show, Read, Eq)

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
  deriving (Show, Read, Eq)

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
  deriving (Show, Read, Eq)

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
  deriving (Show, Read, Eq)

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
  deriving (Show, Read, Eq)

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
  deriving (Show, Read, Eq)

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
  deriving (Show, Read, Eq)

data DisassociateAddressRequest
    = DisassociateAddressRequestEc2 IPv4 -- ^ PublicIp for EC2
    | DisassociateAddressRequestVpc IPv4
      -- ^ AssociationId for VPC
  deriving (Show, Read, Eq)

data SecurityGroupRequest
    = SecurityGroupRequestGroupId Text
    | SecurityGroupRequestGroupName Text
  deriving (Show, Read, Eq)

data Subnet = Subnet
    { subnetId :: Text
    , subnetState :: SubnetState
    , subnetVpicId :: Text
    , subnetCidrBlock :: AddrRange IPv4
    , subnetAvailableIpAddressCount :: Int
    , subnetAvailabilityZone :: Text
    , subnetDefaultForAz :: Maybe Bool
    , subnetMapPublicIpOnLaunch :: Maybe Bool
    , subnetTagSet :: [ResourceTag]
    }
  deriving (Show, Read, Eq)

data SubnetState = SubnetStatePending | SubnetStateAvailable
  deriving (Show, Read, Eq)

data CreateSubnetRequest = CreateSubnetRequest
    { createSubnetRequestVpcId :: Text
    , createSubnetRequestCidrBlock :: AddrRange IPv4
    , createSubnetRequestAvailabilityZone :: Maybe Text
    }
  deriving (Show, Read, Eq)

data VolumeStatus = VolumeStatus
    { volumeStatusVolumeId :: Text
    , volumeStatusAvailabilityZone :: Text
    , volumeStatusVolumeStatus :: VolumeStatusInfo
    , volumeStatusEventSet :: [VolumeStatusEvent]
    , volumeStatusActionSet :: [VolumeStatusAction]
    }
  deriving (Show, Read, Eq)

data VolumeStatusInfo = VolumeStatusInfo
    { volumeStatusInfoStatus :: VolumeStatusInfoStatus
    , volumeStatusInfoDetails :: [VolumeStatusDetail]
    }
  deriving (Show, Read, Eq)

data VolumeStatusInfoStatus
    = VolumeStatusInfoStatusOK
    | VolumeStatusInfoStatusImpaired
    | VolumeStatusInfoStatusInsufficientData
  deriving (Show, Read, Eq)

data VolumeStatusDetail = VolumeStatusDetail
    { volumeStatusDetailName :: Text
    , volumeStatusDetailStatus :: Text
    }
  deriving (Show, Read, Eq)

data VolumeStatusEvent = VolumeStatusEvent
    { volumeStatusEventType :: Text
    , volumeStatusEventId :: Text
    , volumeStatusEventDescription :: Text
    , volumeStatusEventNotBefore :: Maybe UTCTime
    , volumeStatusEventNotAfter :: Maybe UTCTime
    }
  deriving (Show, Read, Eq)

data VolumeStatusAction = VolumeStatusAction
    { volumeStatusActionCode :: Text
    , volumeStatusActionEventType :: Text
    , volumeStatusActionEventId :: Text
    , volumeStatusActionDescription :: Text
    }
  deriving (Show, Read, Eq)

data VolumeAttribute
    = VolumeAttributeAutoEnableIO Bool
    | VolumeAttributeProductCodes [ProductCode]
  deriving (Show, Read, Eq)

data VolumeAttributeRequest
    = VolumeAttributeRequestAutoEnableIO
    | VolumeAttributeRequestProductCodes
  deriving (Show, Read, Eq)

data NetworkAcl = NetworkAcl
    { networkAclId :: Text
    , networkAclVpcId :: Text
    , networkAclDefault :: Bool
    , networkAclEntrySet :: [NetworkAclEntry]
    , networkAclAssociationSet :: [NetworkAclAssociation]
    , networkAclTagSet :: [ResourceTag]
    }
  deriving (Show, Read, Eq)

data NetworkAclEntry = NetworkAclEntry
    { networkAclEntryRuleNumber :: Int
    , networkAclEntryProtocol :: Int
    , networkAclEntryRuleAction :: NetworkAclRuleAction
    , networkAclEntryEgress :: Bool
    , networkAclEntryCidrBlock :: Text
    , networkAclEntryIcmpTypeCode :: Maybe IcmpTypeCode
    , networkAclEntryPortRange :: Maybe PortRange
    }
  deriving (Show, Read, Eq)

data NetworkAclRuleAction
    = NetworkAclRuleActionAllow
    | NetworkAclRuleActionDeny
  deriving (Show, Read, Eq)

data IcmpTypeCode = IcmpTypeCode
    { icmpTypeCodeCode :: Int
    , icmpTypeCodeType :: Int
    }
  deriving (Show, Read, Eq)

data PortRange = PortRange
    { portRangeFrom :: Int
    , portRangeTo :: Int
    }
  deriving (Show, Read, Eq)

data NetworkAclAssociation = NetworkAclAssociation
    { networkAclAssociationId :: Text
    , networkAclAssociationNetworkAclId :: Text
    , networkAclAssociationSubnetId :: Text
    }
  deriving (Show, Read, Eq)

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
  deriving (Show, Read, Eq)

data RouteTable = RouteTable
    { routeTableId :: Text
    , routeTableVpcId :: Text
    , routeTableRouteSet :: [Route]
    , routeTableAssociationSet :: [RouteTableAssociation]
    , routeTablePropagatingVgw :: Maybe PropagatingVgw
    , routeTableTagSet :: [ResourceTag]
    }
  deriving (Show, Read, Eq)

data Route = Route
    { routeDestinationCidrBlock :: Text
    , routeGatewayId :: Maybe Text
    , routeInstanceId :: Maybe Text
    , routeInstanceOwnerId :: Maybe Text
    , routeNetworkInterfaceId :: Maybe Text
    , routeState :: RouteState
    , routeOrigin :: Maybe RouteOrigin
    }
  deriving (Show, Read, Eq)

data RouteState = RouteStateActive | RouteStateBlackhole
  deriving (Show, Read, Eq)

data RouteOrigin
    = RouteOriginCreateRouteTable
    | RouteOriginCreateRoute
    | RouteOriginTableEnableVgwRoutePropagation
  deriving (Show, Read, Eq)

data RouteTableAssociation = RouteTableAssociation
    { routeTableAssociationId :: Text
    , routeTableAssociationRouteTableId :: Text
    , routeTableAssociationSubnetId :: Maybe Text
    , routeTableAssociationMain :: Maybe Bool
    }
  deriving (Show, Read, Eq)

data CreateRouteRequest
    = CreateRouteToGateway
        { createRouteTableId :: Text
        , createRouteDestinationCidrBlock :: AddrRange IPv4
        , createRouteGatewayId :: Text
        }
    | CreateRouteToInstance
        { createRouteTableId :: Text
        , createRouteDestinationCidrBlock :: AddrRange IPv4
        , createRouteInstanceId :: Text
        }
    | CreateRouteToNetworkInterface
        { createRouteTableId :: Text
        , createRouteDestinationCidrBlock :: AddrRange IPv4
        , createRouteNetworkInterfaceId :: Text
        }
  deriving (Show, Read, Eq)

type PropagatingVgw = Text

data Vpc = Vpc
    { vpcId :: Text
    , vpcState :: VpcState
    , vpcCidrBlock :: AddrRange IPv4
    , vpcDhcpOptionsId :: Text
    , vpcTagSet :: [ResourceTag]
    , vpcInstanceTenancy :: Text
    , vpcIsDefault :: Maybe Text
    }
  deriving (Show, Read, Eq)

data VpcState = VpcStatePending | VpcStateAvailable
  deriving (Show, Read, Eq)

data VpnGateway = VpnGateway
    { vpnGatewayId :: Text
    , vpnGatewayState :: VpnGatewayState
    , vpnGatewayType :: Text
    , vpnGatewayAvailabilityZone :: Maybe Text
    , vpnGatewayAttachments :: [Attachment]
    , vpnGatewayTagSet :: [ResourceTag]
    }
  deriving (Show, Read, Eq)

data VpnGatewayState
    = VpnGatewayStatePending
    | VpnGatewayStateAvailable
    | VpnGatewayStateDeleting
    | VpnGatewayStateDeleted
  deriving (Show, Read, Eq)

data Attachment = Attachment
    { attachmentVpcId :: Text
    , attachmentState :: AttachmentState
    }
  deriving (Show, Read, Eq)

data AttachmentState
    = AttachmentStateAttaching
    | AttachmentStateAttached
    | AttachmentStateDetaching
    | AttachmentStateDetached
  deriving (Show, Read, Eq)

data CreateVpnGatewayType = CreateVpnGatewayTypeIpsec1

data CustomerGateway = CustomerGateway
    { customerGatewayId :: Text
    , customerGatewayState :: CustomerGatewayState
    , customerGatewayType :: Text
    , customerGatewayIpAddress :: IPv4
    , customerGatewayBgpAsn :: Int
    , customerGatewayTagSet :: [ResourceTag]
    }
  deriving (Show, Read, Eq)

data CustomerGatewayState
    = CustomerGatewayStatePending
    | CustomerGatewayStateAvailable
    | CustomerGatewayStateDeleting
    | CustomerGatewayStateDeleted
  deriving (Show, Read, Eq)

data InternetGateway = InternetGateway
    { internetGatewayInternetGatewayId :: Text
    , internetGatewayAttachmentSet :: [InternetGatewayAttachment]
    , internetGatewayTagSet :: [ResourceTag]
    }
  deriving (Show, Read, Eq)

data InternetGatewayAttachment = InternetGatewayAttachment
    { internetGatewayAttachmentVpcId :: Text
    , internetGatewayAttachmentState :: InternetGatewayAttachmentState
    }
  deriving (Show, Read, Eq)

data InternetGatewayAttachmentState
    = InternetGatewayAttachmentStateAttaching
    | InternetGatewayAttachmentStateAttached
    | InternetGatewayAttachmentStateDetaching
    | InternetGatewayAttachmentStateDetached
    | InternetGatewayAttachmentStateAvailable
  deriving (Show, Read, Eq)

data DhcpOptions = DhcpOptions
    { dhcpOptionsId :: Text
    , dhcpOptionsDhcpConfigurationSet :: [DhcpConfiguration]
    , dhcpOptionsTagSet :: [ResourceTag]
    }
  deriving (Show, Read, Eq)

data DhcpConfiguration = DhcpConfiguration
    { dhcpConfigurationKey :: Text
    , dhcpConfigurationDhcpValueSet :: [DhcpValue]
    }
  deriving (Show, Read, Eq)

data DhcpValue = DhcpValue
    { dhcpValueValue :: Text
    }
  deriving (Show, Read, Eq)

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
  deriving (Show, Read, Eq)

type Filter = (Text, [Text])

data NetworkInterfaceStatus
    = NetworkInterfaceStatusAvailable
    | NetworkInterfaceStatusInUse
  deriving (Show, Read, Eq)

data NetworkInterfaceAttachment = NetworkInterfaceAttachment
    { networkInterfaceAttachmentId :: Text
    , networkInterfaceAttachmentInstanceId :: Maybe Text
    , networkInterfaceAttachmentInstanceOwnerId :: Text
    , networkInterfaceAttachmentDeviceIndex :: Int
    , networkInterfaceAttachmentStatus :: Text
    , networkInterfaceAttachmentAttachTime :: UTCTime
    , networkInterfaceAttachmentDeleteOnTermination :: Bool
    }
  deriving (Show, Read, Eq)

data NetworkInterfaceAssociation = NetworkInterfaceAssociation
    { networkInterfaceAssociationAttachmentId :: Maybe Text
    , networkInterfaceAssociationInstanceId :: Maybe Text
    , networkInterfaceAssociationPublicIp :: IPv4
    , networkInterfaceAssociationPublicDnsName :: Maybe Text
    , networkInterfaceAssociationIpOwnerId :: Text
    , networkInterfaceAssociationId :: Text
    }
  deriving (Show, Read, Eq)

data NetworkInterfacePrivateIpAddress
    = NetworkInterfacePrivateIpAddress
    { networkInterfacePrivateIpAddressPrivateIpAddress :: IPv4
    , networkInterfacePrivateIpAddressDnsName :: Maybe Text
    , networkInterfacePrivateIpAddressPrimary :: Bool
    , networkInterfacePrivateIpAddressAssociation
        :: Maybe NetworkInterfaceAssociation
    }
  deriving (Show, Read, Eq)

instance FromText Platform
  where
    fromMaybeText _name Nothing  = return PlatformOther
    fromMaybeText _name (Just t)
        | t == "windows" = return PlatformWindows
        | otherwise      = return PlatformOther

instance FromText AddressDomain
  where
    fromMaybeText _name Nothing  = return AddressDomainStandard
    fromMaybeText _name (Just t)
        | t == "standard" = return AddressDomainStandard
        | t == "vpc"      = return AddressDomainVPC
        | otherwise       = monadThrow $ TextConversionException t

instance FromText EC2Return
  where
    fromTextMay t
        | t == "true" = Just EC2Success
        | otherwise   = Just $ EC2Error t

instance FromText InstanceLifecycle
  where
    fromMaybeText _name Nothing  = return LifecycleNone
    fromMaybeText _name (Just t)
        | t == "spot" = return LifecycleSpot
        | otherwise   = monadThrow $ TextConversionException t

deriveFromText "ImageState" ["available", "pending", "failed"]
deriveFromText "ProductCodeType" ["devpay", "marketplace"]
deriveFromText "ImageType" ["machine", "kernel", "ramdisk"]
deriveFromText "RootDeviceType" ["ebs", "instance-store"]
deriveFromText "VirtualizationType" ["paravirtual", "hvm"]
deriveFromText "Hypervisor" ["ovm", "xen"]
deriveFromText "InstanceStatusEventCode"
    [ "instance-reboot"
    , "instance-stop"
    , "system-reboot"
    , "instance-retirement"
    ]
deriveFromText "InstanceStatusTypeStatus"
    ["ok", "impaired", "insufficient-data", "not-applicable"]
deriveFromText "InstanceMonitoringState"
    ["disabled", "enabled", "pending"]
deriveFromText "Architecture" ["i386", "x86_64"]
deriveFromText "SnapshotStatus" ["pending", "completed", "error"]
deriveFromText "VolumeState"
    [ "creating"
    , "available"
    , "in-use"
    , "deleting"
    , "deleted"
    , "error"
    ]
deriveFromText "AttachmentSetItemResponseStatus"
    ["attaching", "attached", "detaching", "detached"]
deriveFromText "ShutdownBehavior" ["stop", "terminate"]
deriveFromText "VpnConnectionState"
    ["pending", "available", "deleting", "deleted"]
deriveFromText "VpnTunnelTelemetryStatus" ["UP", "DOWN"]
deriveFromText "VpnStaticRouteSource" ["Static"]
deriveFromText "VpnStaticRouteState"
    ["pending", "available", "deleting", "deleted"]
deriveFromText "SubnetState" ["pending", "available"]
deriveFromText "VolumeStatusInfoStatus"
    ["ok", "impaired", "insufficient-data"]
deriveFromText "NetworkAclRuleAction" ["allow", "deny"]
deriveFromText "RouteState" ["active", "blackhole"]
deriveFromText "RouteOrigin"
    [ "CreateRouteTable"
    , "CreateRoute"
    , "EnableVgwRoutePropagation"
    ]
deriveFromText "VpcState" ["pending", "available"]
deriveFromText "VpnGatewayState"
    ["pending", "available", "deleting", "deleted"]
deriveFromText "AttachmentState"
    ["attaching", "attached", "detaching", "detached"]
deriveFromText "CustomerGatewayState"
    ["pending", "available", "deleting", "deleted"]
deriveFromText "InternetGatewayAttachmentState"
    ["attaching", "attached", "detaching", "detached", "available"]
deriveFromText "NetworkInterfaceStatus" ["available", "in-use"]
