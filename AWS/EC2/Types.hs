module AWS.EC2.Types
    ( Address(..)
    , AddressDomain(..)
    , AllocateAddressResponse(..)
    , Architecture(..)
    , AssociateAddressRequest(..)
    , Attachment(..)
    , AttachmentStatus(..)
    , AvailabilityZone(..)
    , AvailabilityZoneMessage
    , BlockDeviceMapping(..)
    , BlockDeviceMappingParam(..)
    , ConsoleOutput(..)
    , CreateVolumeRequest(..)
    , DisassociateAddressRequest(..)
    , EbsBlockDevice(..)
    , EbsSource(..)
    , EC2Return(..)
    , Group(..)
    , Hypervisor(..)
    , IamInstanceProfile(..)
    , Image(..)
    , ImageState(..)
    , ImageType(..)
    , Instance(..)
    , InstanceAttribute(..)
    , InstanceAttributeRequest(..)
    , InstanceBlockDeviceMapping(..)
    , InstanceEbsBlockDevice(..)
    , InstanceLifecycle(..)
    , InstanceMonitoringState(..)
    , InstanceNetworkInterface(..)
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
    , IpRange(..)
    , KeyPair(..)
    , ModifyInstanceAttributeRequest(..)
    , NetworkInterfaceAssociation(..)
    , NetworkInterfaceAttachment(..)
    , NetworkInterfaceParam(..)
    , PasswordData(..)
    , Placement(..)
    , Platform(..)
    , ProductCode(..)
    , ProductCodeType(..)
    , Region(..)
    , RegisterImageRequest(..)
    , Reservation(..)
    , ResetInstanceAttributeRequest(..)
    , ResourceTag(..)
    , RootDeviceType(..)
    , RunInstancesRequest(..)
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
    , VolumeType(..)
    , VolumeStatus(..)
    , VpnConnection(..)
    , VpnConnectionOptionsRequest(..)
    , VpnConnectionState(..)
    , VpnStaticRoute(..)
    , VpnStaticRouteSource(..)
    , VpnStaticRouteState(..)
    , VpnTunnelTelemetry(..)
    , VpnTunnelTelemetryStatus(..)
    ) where

import Data.Default (Default(..))
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Time (UTCTime)

data Image = Image
    { imageId :: Text
    , imageLocation :: Text
    , imageImageState :: ImageState
    , imageOwnerId :: Text
    , isPublic :: Bool
    , imageProductCodes :: [ProductCode]
    , imageArchitecture :: Text
    , imageImageType :: ImageType
    , kernelId :: Maybe Text
    , ramdiskId :: Maybe Text
    , imagePlatform :: Platform
    , imageStateReason :: Maybe StateReason
    , imageViridianEnabled :: Maybe Bool
    , imageOwnerAlias :: Maybe Text
    , imageName :: Text
    , imageDescription :: Text
    , imageBillingProducts :: [Text]
    , imageRootDeviceType :: RootDeviceType
    , imageRootDeviceName :: Maybe Text
    , blockDeviceMappings :: [BlockDeviceMapping]
    , imageVirtualizationType :: VirtualizationType
    , imageTagSet :: [ResourceTag]
    , imageHypervisor :: Hypervisor
    }
  deriving (Show, Eq)

data ImageState
    = ImageAvailable
    | ImagePending
    | ImageFailed
  deriving (Show, Eq)

data ProductCode = ProductCode
    { pcCode :: Text
    , pcType :: ProductCodeType
    }
  deriving (Show, Eq)

data ProductCodeType = Devpay
                     | Marketplace
  deriving (Show, Eq)

data ImageType = Machine
               | Kernel
               | RamDisk
  deriving (Show, Eq)

data Platform = Windows
              | Other
  deriving (Show, Eq)

data StateReason = StateReason
    { stateReasonCode :: Text
    , stateReasonMessage :: Text
    }
  deriving (Show, Eq)

data RootDeviceType
    = EBS
    | InstanceStore
  deriving (Show, Eq)

data BlockDeviceMapping = BlockDeviceMapping
    { deviceName :: Text
    , virtualName :: Maybe Text
    , ebs :: Maybe EbsBlockDevice
    }
  deriving (Show, Eq)

data EbsBlockDevice = EbsBlockDevice
    { ebsSnapshotId :: Maybe Text
    , ebsVolumeSize :: Int
    , ebsDeleteOnTermination :: Bool
    , ebsVolumeType :: VolumeType
    }
  deriving (Show, Eq)

data VolumeType = Standard
                | IO1 Int
  deriving (Show, Eq)

instance Default VolumeType
  where
    def = Standard

data VirtualizationType = Paravirtual
                        | HVM
  deriving (Show, Eq)

data ResourceTag = ResourceTag
    { resourceKey :: Text
    , resourceValue :: Maybe Text
    }
  deriving (Show, Eq)

data Hypervisor = OVM
                | Xen
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
    , messageSet :: [AvailabilityZoneMessage]
    }
  deriving (Show, Eq)

type AvailabilityZoneMessage = Text

data Reservation = Reservation
    { reservationId :: Text
    , ownerId :: Text
    , groupSet :: [Group]
    , instanceSet :: [Instance]
    , requesterId :: Maybe Text
    }
  deriving (Show, Eq)

data Instance = Instance
    { instanceId :: Text
    , instanceImageId :: Text
    , instanceState :: InstanceState
    , privateDnsName :: Text
    , dnsName :: Text
    , reason :: Text
    , instanceKeyName :: Maybe Text
    , amiLaunchIndex :: Text
    , instanceProductCodes :: [ProductCode]
    , instanceType :: Text
    , launchTime :: UTCTime
    , instancePlacement :: Placement
    , instanceKernelId :: Maybe Text
    , instanceRamdiskId :: Maybe Text
    , instancePlatform :: Maybe Text
    , instanceMonitoring :: InstanceMonitoringState
    , subnetId :: Maybe Text
    , vpcId :: Maybe Text
    , privateIpAddress :: Maybe Text
    , ipAddress :: Maybe Text
    , sourceDestCheck :: Maybe Bool
    , vpcGroupSet :: [Group]
    , instanceStateReason :: Maybe StateReason
    , instanceArchitecture :: Architecture
    , instanceRootDeviceType :: RootDeviceType
    , instanceRootDeviceName :: Maybe Text
    , instanceBlockDeviceMappings :: [InstanceBlockDeviceMapping]
    , instanceInstanceLifecycle :: InstanceLifecycle
    , spotInstanceRequestId :: Maybe Text
    , instanceVirtualizationType :: VirtualizationType
    , clientToken :: Text
    , instanceTagSet :: [ResourceTag]
    , instanceHypervisor :: Hypervisor
    , instanceNetworkInterfaceSet :: [InstanceNetworkInterface]
    , instanceIamInstanceProfile :: Maybe IamInstanceProfile
    , ebsOptimized :: Bool -- default: false
    }
  deriving (Show, Eq)

data InstanceStatus = InstanceStatus
    { isInstanceId :: Text
    , isAvailabilityZone :: Text
    , isEventsSet :: [InstanceStatusEvent]
    , isInstanceState :: InstanceState
    , isSystemStatus :: InstanceStatusType
    , isInstanceStatus :: InstanceStatusType
    }
  deriving (Show, Eq)

data InstanceStatusEvent = InstanceStatusEvent
    { iseCode :: InstanceStatusEventCode
    , iseDescription :: Text
    , iseNotBefore :: Maybe UTCTime
    , iseNotAfter :: Maybe UTCTime
    }
  deriving (Show, Eq)

data InstanceStatusEventCode
    = InstanceReboot
    | InstanceStop
    | SystemReboot
    | InstanceRetirement
  deriving (Show, Eq)

data InstanceStatusType = InstanceStatusType
    { isdStatus :: InstanceStatusTypeStatus
    , isdDetails :: [InstanceStatusDetail]
    }
  deriving (Show, Eq)

data InstanceStatusTypeStatus
    = InstanceStatusOK
    | InstanceStatusImpaired
    | InstanceStatusInsufficientData
    | InstanceStatusNotApplicable
  deriving (Show, Eq)

data InstanceStatusDetail = InstanceStatusDetail
    { isddName :: InstanceStatusDetailName
    , isddStatus :: InstanceStatusDetailStatus
    , isddImpairedSince :: Maybe UTCTime
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
    = Pending
    | Running
    | ShuttingDown
    | Terminated
    | Stopping
    | Stopped
    | UnknownState Int
  deriving (Show, Eq)

data Placement = Placement
    { placementAvailabilityZone :: Text
    , placementGroupName :: Text
    , tenancy :: Text
    }
  deriving (Show, Eq)

data InstanceMonitoringState
    = MonitoringDisabled
    | MonitoringEnabled
    | MonitoringPending
  deriving (Show, Eq)

data Architecture = I386 | X86_64 deriving (Show, Eq)

data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping
    { instanceDeviceName :: Text
    , instanceEbs :: InstanceEbsBlockDevice
    }
  deriving (Show, Eq)

data InstanceEbsBlockDevice = InstanceEbsBlockDevice
    { instanceEbsVolumeId :: Text
    , instanceEbsState :: AttachmentStatus
    , instanceEbsAttachTime :: UTCTime
    , instanceEbsDeleteOnTermination :: Bool
    }
  deriving (Show, Eq)

data InstanceLifecycle = LifecycleSpot | LifecycleNone
  deriving (Show, Eq)

data InstanceNetworkInterface = InstanceNetworkInterface
    { instanceNetworkInterfaceId :: Text
    , iniSubnetId :: Text
    , iniVpcId :: Text
    , iniDescription :: Text
    , iniOwnerId :: Text
    , iniStatus :: Text
    , iniPrivateIpAddress :: Text
    , iniPrivateDnsName :: Maybe Text
    , iniSourceDestCheck :: Bool
    , iniGroupSet :: [Group]
    , iniAttachment :: NetworkInterfaceAttachment
    , iniAssociation :: Maybe NetworkInterfaceAssociation
    , iniPrivateIpAddressSet :: [InstancePrivateIpAddress]
    }
  deriving (Show, Eq)

data NetworkInterfaceAttachment = NetworkInterfaceAttachment
    { niatAttachmentId :: Text
    , niatDeviceIndex :: Int
    , niatStatus :: Text
    , niatAttachTime :: UTCTime
    , niatDeleteOnTermination :: Bool
    }
  deriving (Show, Eq)

data NetworkInterfaceAssociation = NetworkInterfaceAssociation
    { niasPublicIp :: Text
    , niasIpOwnerId :: Text
    }
  deriving (Show, Eq)

data InstancePrivateIpAddress = InstancePrivateIpAddress
    { iPrivateIpAddress :: Text
    , iPrimary :: Bool
    , iAssociation :: Maybe NetworkInterfaceAssociation
    }
  deriving (Show, Eq)

data IamInstanceProfile = IamInstanceProfile
    { iipArn :: Text
    , iipId :: Text
    }
  deriving (Show, Eq)

data ShutdownBehavior
    = SBStop
    | SBTerminate
  deriving (Show, Eq)

data InstanceAttribute
    = IAInstanceType Text
    | IAKernelId (Maybe Text)
    | IARamdiskId (Maybe Text)
    | IAUserData (Maybe Text)
    | IADisableApiTermination Bool
    | IAShutdownBehavior ShutdownBehavior
    | IARootDeviceName (Maybe Text)
    | IABlockDeviceMapping [InstanceBlockDeviceMapping]
    | IASourceDestCheck (Maybe Bool)
    | IAGroupSet [Text]
    | IAProductCodes [ProductCode]
    | IAEbsOptimized Bool
  deriving (Show, Eq)

data Address = Address
    { addrPublicIp :: Text
    , addrAllocationId :: Maybe Text
    , addrDomain :: AddressDomain
    , addrInstanceId :: Maybe Text
    , addrAssociationId :: Maybe Text
    , addrNetworkInterfaceId :: Maybe Text
    , addrNetworkInterfaceOwnerId :: Maybe Text
    , addrPrivateIpAddress :: Maybe Text
    }
  deriving (Show, Eq)

data AddressDomain = AddressDomainStandard | AddressDomainVPC
  deriving (Show, Eq)

data AllocateAddressResponse = AllocateAddressResponse
    { alaPublicIp :: Text
    , alaDomain :: AddressDomain
    , alaAllocationId :: Maybe Text
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
    { iscInstanceId :: Text
    , iscCurrentState :: InstanceState
    , iscPreviousState :: InstanceState
    }
  deriving (Show, Eq)

data ConsoleOutput = ConsoleOutput
    { coInstanceId :: Text
    , coTimestamp :: UTCTime -- ^ The time the data was last updated.
    , coOutput :: Text
    }
  deriving (Show, Eq)

data PasswordData = PasswordData
    { pdInstanceId :: Text
    , pdTimestamp :: UTCTime -- ^ The time the data was last updated.
    , pdPasswordData :: Text
    }
  deriving (Show, Eq)

data Snapshot = Snapshot
    { snapshotId :: Text
    , ssVolumeId :: Text
    , ssStatus :: SnapshotStatus
    , ssStartTime :: UTCTime
    , ssProgress :: Text
    , ssOwnerId :: Text
    , ssVolumeSize :: Int
    , ssDescription :: Text
    , ssOwnerAlias :: Maybe Text
    , ssTagSet :: [ResourceTag]
    }
  deriving (Show, Eq)

data SnapshotStatus = SSPending | SSCompleted | SSError
  deriving (Show, Eq)

data Volume = Volume
    { volumeId :: Text
    , volSize :: Int
    , volSnapshotId :: Maybe Text
    , volAvailabilityZone :: Text
    , volStatus :: VolumeStatus
    , volCreateTime :: UTCTime
    , volAttachmentSet :: [Attachment]
    , volTagSet :: [ResourceTag]
    , volVolumeType :: VolumeType
    }
  deriving (Show, Eq)

data VolumeStatus
    = VolCreating
    | VolAvailable
    | VolInUse
    | VolDeleting
    | VolDeleted
    | VolError
  deriving (Show, Eq)

data Attachment = Attachment
    { attVolumeId :: Text
    , attInstanceId :: Text
    , attDevice :: Text
    , attStatus :: AttachmentStatus
    , attAttachTime :: UTCTime
    , attDeleteOnTermination :: Maybe Bool
    }
  deriving (Show, Eq)

data AttachmentStatus
    = AttAttaching
    | AttAttached
    | AttDetaching
    | AttDetached
  deriving (Show, Eq)

data KeyPair = KeyPair
    { keyName :: Text
    , keyFingerprint :: Text
    }
  deriving (Show, Eq)

data SecurityGroup = SecurityGroup
    { sgOwnerId :: Text
    , sgGroupId :: Text
    , sgGroupName :: Text
    , sgGroupDescription :: Text
    , sgVpcId :: Maybe Text
    , sgIpPermissions :: [IpPermission]
    , sgIpPermissionsEgress :: [IpPermission]
    , sgTagSet :: [ResourceTag]
    }
  deriving (Show, Eq)

data IpPermission = IpPermission
    { ippIpProtocol :: Text
    , ippFromPort :: Maybe Int
    , ippToPort :: Maybe Int
    , ippGroups :: [UserIdGroupPair]
    , ippIpRanges :: [IpRange]
    }
  deriving (Show, Eq)

data UserIdGroupPair = UserIdGroupPair
    { uigpUserId :: Maybe Text
    , uigpGroupId :: Text
    , uigpGroupName :: Maybe Text
    }
  deriving (Show, Eq)

data IpRange = IpRange
    { iprCidrIp :: Text
    }
  deriving (Show, Eq)

data BlockDeviceMappingParam
    = BlockDeviceMappingParamEBS
        { bdmpEbsDeviceName :: Text
        , bdmpEbsNoDevice :: Maybe Bool
        , bdmpEbsSource :: EbsSource
        , bdmpEbsDeleteOnTermination :: Maybe Bool
        , bdmpEbsVolumeType :: Maybe VolumeType
        }
    | BlockDeviceMappingParamInstanceStore
        { bdmpIsDeviceName :: Text
        , bdmpIsNoDevice :: Maybe Bool
        , bdmpIsVirtualName :: Maybe Text
        }
  deriving (Show, Eq)

data EbsSource
    = EbsSnapshotId Text
    | EbsVolumeSize Int
  deriving (Show, Eq)

data NetworkInterfaceParam = NetworkInterfaceParam
    { nipInterfaceId :: Maybe Text
    , nipDeviceIndex :: Maybe Text
    , nipSubnetId :: Maybe Text
    , nipDescription :: Maybe Text
    , nipPrivateIpAddresses :: [Text]
    , nipSecurityGroupIds :: [Text]
    , nipDeleteOnTermination :: Maybe Bool
    }
  deriving (Show, Eq)

data VpnConnection = VpnConnection
    { vpnConnectionId :: Text
    , vcState :: VpnConnectionState
    , vcCustomerGatewayConfiguration :: Text
    , vcType :: Text
    , vcCustomerGatewayId :: Text
    , vcVpnGatewayId :: Text
    , vcTagSet :: [ResourceTag]
    , vcVgwTelemetry :: [VpnTunnelTelemetry]
    , vcOptions :: Maybe VpnConnectionOptionsRequest
    , vcRoutes :: Maybe VpnStaticRoute
    }
  deriving (Show, Eq)

data VpnConnectionState
    = VCSPending
    | VCSAvailable
    | VCSDeleting
    | VCSDeleted
  deriving (Show, Eq)

data VpnTunnelTelemetry = VpnTunnelTelemetry
    { vttOutsideIpAddress :: Text
    , vttStatus :: VpnTunnelTelemetryStatus
    , vttLastStatusChange :: UTCTime
    , vttStatusMessage :: Text
    , vttAcceptRouteCount :: Int
    }
  deriving (Show, Eq)

data VpnTunnelTelemetryStatus
    = VTTSUp
    | VTTSDown
  deriving (Show, Eq)

data VpnConnectionOptionsRequest = VpnConnectionOptionsRequest
    { staticRoutesOnly :: Bool
    }
  deriving (Show, Eq)

data VpnStaticRoute = VpnStaticRoute
    { vsrDestinationCidrBlock :: Text
    , vsrSource :: VpnStaticRouteSource
    , vsrState :: VpnStaticRouteState
    }
  deriving (Show, Eq)

data VpnStaticRouteSource = VSRStatic
  deriving (Show, Eq)

data VpnStaticRouteState
    = VSRSPending
    | VSRSAvailable
    | VSRSDeleting
    | VSRSDeleted
  deriving (Show, Eq)

data RunInstancesRequest = RunInstancesRequest
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
  deriving (Show, Eq)

data InstanceAttributeRequest
    = IARInstanceType
    | IARKernelId
    | IARRamdiskId
    | IARUserData
    | IARDisableApiTermination
    | IARShutdownBehavior
    | IARRootDeviceName
    | IARBlockDeviceMapping
    | IARSourceDestCheck
    | IARGroupSet
    | IARProductCodes
    | IAREbsOptimized
  deriving (Show, Eq, Ord)

data ResetInstanceAttributeRequest
    = RIAPKernel
    | RIAPRamdisk
    | RIAPSourceDestCheck
  deriving (Show, Eq)

data ModifyInstanceAttributeRequest
    = MIAPInstanceType Text
    | MIAPKernelId Text
    | MIAPRamdiskId Text
    | MIAPUserData Text
    | MIAPDisableApiTermination Bool
    | MIAPShutdownBehavior ShutdownBehavior
    | MIAPRootDeviceName Text
    | MIAPBlockDeviceMapping [BlockDeviceMappingParam]
    | MIAPSourceDestCheck Bool
    | MIAPGroupSet [Text]
    | MIAPEbsOptimized Bool
  deriving (Show, Eq)

data RegisterImageRequest = RegisterImageRequest
    { rirName :: Text
    , rirImageLocation :: Maybe Text
    , rirDescription :: Maybe Text
    , rirArchitecture :: Maybe Text
    , rirKernelId :: Maybe Text
    , rirRamdiskId :: Maybe Text
    , rirRootDeviceName :: Maybe Text
    , rirBlockDeviceMappings :: [BlockDeviceMappingParam]
    }
  deriving (Show, Eq)

data CreateVolumeRequest
    = CreateNewVolume
        { cnvSize :: Int
        , cnvAvailabilityZone :: Text
        , cnvVolumeType :: Maybe VolumeType
        }
    | CreateFromSnapshot
        { cfsSnapshotId :: Text
        , cfsAvailabilityZone :: Text
        , cfsSize :: Maybe Int
        , cfsVolumeType :: Maybe VolumeType
        }
  deriving (Show, Eq)

data AssociateAddressRequest
    = AAEC2Instance
        { aaec2PublicIp :: Text
        , aaec2InstanceId :: Text
        }
    | AAVPCInstance
        { aavpcAllocationId :: Text
        , aavpcInstanceId :: Maybe Text
        , aavpcNetworkInterfaceId :: Maybe Text
        , aavpcPrivateIpAddress :: Maybe Text
        , aavpcAllowReassociation :: Maybe Bool
        }
  deriving (Show, Eq)

data DisassociateAddressRequest
    = DAEC2 Text -- ^ PublicIp for EC2
    | DAVPC Text -- ^ AssociationId for VPC
  deriving (Show, Eq)

data SecurityGroupRequest = GroupId Text | GroupName Text
  deriving (Show, Eq)

data Subnet = Subnet
    { snSubnetId :: Text
    , snState :: SubnetState
    , snVpicId :: Text
    , snCidrBlock :: Text
    , snAvailableIpAddressCount :: Int
    , snCAvailabilityZone :: Text
    , snTagSet :: [ResourceTag]
    }
  deriving (Show, Eq)

data SubnetState = SubnetPending | SubnetAvailable
  deriving (Show, Eq)
