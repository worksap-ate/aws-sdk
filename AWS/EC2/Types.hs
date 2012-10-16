module AWS.EC2.Types where

import Data.Default (Default(..))
import Data.Text (Text)
import Data.Time (UTCTime)

import AWS.Util

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
  deriving (Show)

data ImageState
    = ImageAvailable
    | ImagePending
    | ImageFailed
  deriving (Show)

imageState :: Text -> ImageState
imageState a
    | a == "available" = ImageAvailable
    | a == "pending"   = ImagePending
    | a == "failed"    = ImageFailed
    | otherwise        = err "image state" a

data ProductCode = ProductCode
    { pcCode :: Text
    , pcType :: ProductCodeType
    }
  deriving (Show)

data ProductCodeType = Devpay
                     | Marketplace
  deriving (Show)

productCodeType :: Text -> ProductCodeType
productCodeType t
    | t == "marketplace" = Marketplace
    | t == "devpay"      = Devpay
    | otherwise          = err "product code type" t

data ImageType = Machine
               | Kernel
               | RamDisk
  deriving (Show)

imageType :: Text -> ImageType
imageType t
    | t == "machine"  = Machine
    | t == "kernel"   = Kernel
    | t == "ramdisk" = RamDisk
    | otherwise       = err "image type" t

data Platform = Windows
              | Other
  deriving (Show)

platform :: Maybe Text -> Platform
platform Nothing   = Other
platform (Just t)
    | t == "windows" = Windows
    | otherwise      = Other

data StateReason = StateReason
    { stateReasonCode :: Text
    , stateReasonMessage :: Text
    }
  deriving (Show)

data RootDeviceType
    = EBS
    | InstanceStore
  deriving (Show)

rootDeviceType :: Text -> RootDeviceType
rootDeviceType t
    | t == "ebs"            = EBS
    | t == "instance-store" = InstanceStore
    | otherwise             = err "root device type" t

data BlockDeviceMapping = BlockDeviceMapping
    { deviceName :: Text
    , virtualName :: Maybe Text
    , ebs :: Maybe EbsBlockDevice
    }
  deriving (Show)

data EbsBlockDevice = EbsBlockDevice
    { ebsSnapshotId :: Maybe Text
    , ebsVolumeSize :: Int
    , ebsDeleteOnTermination :: Bool
    , ebsVolumeType :: VolumeType
    }
  deriving (Show)

data VolumeType = Standard
                | IO1 Int
  deriving (Show)

volumeType :: Text -> Maybe Int -> VolumeType
volumeType t Nothing  | t == "standard" = Standard
volumeType t (Just i) | t == "io1"      = IO1 i
volumeType t _ = err "volume type" t

instance Default VolumeType
  where
    def = Standard

data VirtualizationType = Paravirtual
                        | HVM
  deriving (Show)

virtualizationType :: Text -> VirtualizationType
virtualizationType t
    | t == "paravirtual" = Paravirtual
    | t == "hvm"         = HVM
    | otherwise          = err "virtualization type" t

data ResourceTag = ResourceTag
    { resourceKey :: Text
    , resourceValue :: Maybe Text
    }
  deriving (Show)

data Hypervisor = OVM
                | Xen
  deriving (Show)

hypervisor :: Text -> Hypervisor
hypervisor t
    | t == "xen" = Xen
    | t == "ovm" = OVM
    | otherwise  = err "hypervisor" t

{- DescribeRegions -}
data Region = Region
    { regionName :: Text
    , regionEndpoint :: Text
    }
  deriving (Show)

data AvailabilityZone = AvailabilityZone
    { zoneName :: Text
    , zoneState :: Text
    , zoneRegionName :: Text
    , messageSet :: [AvailabilityZoneMessage]
    }
  deriving (Show)

type AvailabilityZoneMessage = Text

data Reservation = Reservation
    { reservationId :: Text
    , ownerId :: Text
    , groupSet :: [Group]
    , instanceSet :: [Instance]
    , requesterId :: Maybe Text
    }
  deriving (Show)

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
  deriving (Show)

data InstanceStatus = InstanceStatus
    { isInstanceId :: Text
    , isAvailabilityZone :: Text
    , isEventsSet :: [InstanceStatusEvent]
    , isInstanceState :: InstanceState
    , isSystemStatus :: InstanceStatusType
    , isInstanceStatus :: InstanceStatusType
    }
  deriving (Show)

data InstanceStatusEvent = InstanceStatusEvent
    { iseCode :: InstanceStatusEventCode
    , iseDescription :: Text
    , iseNotBefore :: Maybe UTCTime
    , iseNotAfter :: Maybe UTCTime
    }
  deriving (Show)

data InstanceStatusEventCode
    = InstanceReboot
    | InstanceStop
    | SystemReboot
    | InstanceRetirement
  deriving (Show)

instanceStatusEventCode :: Text -> InstanceStatusEventCode
instanceStatusEventCode t
    | t == "instance-reboot"     = InstanceReboot
    | t == "instance-stop"       = InstanceStop
    | t == "system-reboot"       = SystemReboot
    | t == "instance-retirement" = InstanceRetirement
    | otherwise                  = err "InstanceStatusEventCode" t

data InstanceStatusType = InstanceStatusType
    { isdStatus :: InstanceStatusTypeStatus
    , isdDetails :: [InstanceStatusDetail]
    }
  deriving (Show)

data InstanceStatusTypeStatus
    = InstanceStatusOK
    | InstanceStatusImpaired
    | InstanceStatusInsufficientData
    | InstanceStatusNotApplicable
  deriving (Show)

instanceStatusTypeStatus :: Text -> InstanceStatusTypeStatus
instanceStatusTypeStatus t
    | t == "ok"                = InstanceStatusOK
    | t == "impaired"          = InstanceStatusImpaired
    | t == "insufficient-data" = InstanceStatusInsufficientData
    | t == "not-applicable"    = InstanceStatusNotApplicable
    | otherwise = err "instance status detail status" t

data InstanceStatusDetail = InstanceStatusDetail
    { isddName :: InstanceStatusDetailName
    , isddStatus :: InstanceStatusDetailStatus
    , isddImpairedSince :: Maybe UTCTime
    }
  deriving (Show)

type InstanceStatusDetailName = Text

type InstanceStatusDetailStatus = Text

data Group = Group
    { groupId :: Text
    , groupName :: Text
    }
  deriving (Show)

data InstanceState
    = Pending
    | Running
    | ShuttingDown
    | Terminated
    | Stopping
    | Stopped
    | UnknownState Int
  deriving (Show)

instanceStateCodes :: [(Int, InstanceState)]
instanceStateCodes =
    [ (0, Pending)
    , (16, Running)
    , (32, ShuttingDown)
    , (48, Terminated)
    , (64, Stopping)
    , (80, Stopped)
    ]

codeToState :: Int -> InstanceState
codeToState code = case lookup code instanceStateCodes of
    Nothing -> UnknownState code
    Just st -> st

data Placement = Placement
    { placementAvailabilityZone :: Text
    , placementGroupName :: Text
    , tenancy :: Text
    }
  deriving (Show)

data InstanceMonitoringState
    = MonitoringDisabled
    | MonitoringEnabled
    | MonitoringPending
  deriving (Show)

instanceMonitoringState :: Text -> InstanceMonitoringState
instanceMonitoringState t
    | t == "disabled" = MonitoringDisabled
    | t == "enabled"  = MonitoringEnabled
    | t == "pending"  = MonitoringPending
    | otherwise       = err "monitoring state" t

data Architecture = I386 | X86_64 deriving (Show)

architecture :: Text -> Architecture
architecture t
    | t == "i386"   = I386
    | t == "x86_64" = X86_64
    | otherwise     = err "architecture" t

data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping
    { instanceDeviceName :: Text
    , instanceEbs :: InstanceEbsBlockDevice
    }
  deriving (Show)

data InstanceEbsBlockDevice = InstanceEbsBlockDevice
    { instanceEbsVolumeId :: Text
    , instanceEbsState :: AttachmentStatus
    , instanceEbsAttachTime :: UTCTime
    , instanceEbsDeleteOnTermination :: Bool
    }
  deriving (Show)

data InstanceLifecycle = LifecycleSpot | LifecycleNone
  deriving (Show)

instanceLifecycle :: Maybe Text -> InstanceLifecycle
instanceLifecycle Nothing = LifecycleNone
instanceLifecycle (Just t)
    | t == "spot"   = LifecycleSpot
    | otherwise     = err "lifecycle" t

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
  deriving (Show)

data NetworkInterfaceAttachment = NetworkInterfaceAttachment
    { niatAttachmentId :: Text
    , niatDeviceIndex :: Int
    , niatStatus :: Text
    , niatAttachTime :: UTCTime
    , niatDeleteOnTermination :: Bool
    }
  deriving (Show)

data NetworkInterfaceAssociation = NetworkInterfaceAssociation
    { niasPublicIp :: Text
    , niasIpOwnerId :: Text
    }
  deriving (Show)

data InstancePrivateIpAddress = InstancePrivateIpAddress
    { iPrivateIpAddress :: Text
    , iPrimary :: Bool
    , iAssociation :: Maybe NetworkInterfaceAssociation
    }
  deriving (Show)

data IamInstanceProfile = IamInstanceProfile
    { iipArn :: Text
    , iipId :: Text
    }
  deriving (Show)

data ShutdownBehavior
    = SBStop
    | SBTerminate
  deriving (Show)

shutdownBehavior :: Text -> ShutdownBehavior
shutdownBehavior t
    | t == "stop"      = SBStop
    | t == "terminate" = SBTerminate
    | otherwise = err "shutdown behavior" t

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
  deriving (Show)

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
  deriving (Show)

data AddressDomain = AddressDomainStandard | AddressDomainVPC
  deriving (Show)

addressDomain :: Maybe Text -> AddressDomain
addressDomain Nothing = AddressDomainStandard
addressDomain (Just t)
    | t == "standard" = AddressDomainStandard
    | t == "vpc"      = AddressDomainVPC
    | otherwise       = err "address domain" t

data AllocateAddressResponse = AllocateAddressResponse
    { alaPublicIp :: Text
    , alaDomain :: AddressDomain
    , alaAllocationId :: Maybe Text
    }
  deriving (Show)

data EC2Return = EC2Success | EC2Error Text
  deriving (Show)

ec2Return :: Text -> EC2Return
ec2Return t
    | t == "true" = EC2Success
    | otherwise   = EC2Error t

data Tag = Tag
    { tagResourceId :: Text
    , tagResourceType :: Text
    , tagKey :: Text
    , tagValue :: Maybe Text
    }
  deriving (Show)

data InstanceStateChange = InstanceStateChange
    { iscInstanceId :: Text
    , iscCurrentState :: InstanceState
    , iscPreviousState :: InstanceState
    }
  deriving (Show)

data ConsoleOutput = ConsoleOutput
    { coInstanceId :: Text
    , coTimestamp :: UTCTime -- ^ The time the data was last updated.
    , coOutput :: Text
    }
  deriving (Show)

data PasswordData = PasswordData
    { pdInstanceId :: Text
    , pdTimestamp :: UTCTime -- ^ The time the data was last updated.
    , pdPasswordData :: Text
    }
  deriving (Show)

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
  deriving (Show)

data SnapshotStatus = SSPending | SSCompleted | SSError
  deriving (Show)

snapshotStatus :: Text -> SnapshotStatus
snapshotStatus t
    | t == "pending"   = SSPending
    | t == "completed" = SSCompleted
    | t == "error"     = SSError
    | otherwise        = err "snapshot status" t

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
  deriving (Show)

data VolumeStatus
    = VolCreating
    | VolAvailable
    | VolInUse
    | VolDeleting
    | VolDeleted
    | VolError
  deriving (Show)

volumeStatus :: Text -> VolumeStatus
volumeStatus t
    | t == "creating"  = VolCreating
    | t == "available" = VolAvailable
    | t == "in-use"    = VolInUse
    | t == "deleting"  = VolDeleting
    | t == "deleted"   = VolDeleted
    | t == "error"     = VolError
    | otherwise        = err "volume state" t

data Attachment = Attachment
    { attVolumeId :: Text
    , attInstanceId :: Text
    , attDevice :: Text
    , attStatus :: AttachmentStatus
    , attAttachTime :: UTCTime
    , attDeleteOnTermination :: Maybe Bool
    }
  deriving (Show)

data AttachmentStatus
    = AttAttaching
    | AttAttached
    | AttDetaching
    | AttDetached
  deriving (Show)

attachmentStatus :: Text -> AttachmentStatus
attachmentStatus t
    | t == "attaching" = AttAttaching
    | t == "attached"  = AttAttached
    | t == "detaching" = AttDetaching
    | t == "detached"  = AttDetached
    | otherwise        = err "attachment status" t

data KeyPair = KeyPair
    { keyName :: Text
    , keyFingerprint :: Text
    }
  deriving (Show)

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
  deriving (Show)

data IpPermission = IpPermission
    { ippIpProtocol :: Text
    , ippFromPort :: Maybe Int
    , ippToPort :: Maybe Int
    , ippGroups :: [UserIdGroupPair]
    , ippIpRanges :: [IpRange]
    }
  deriving (Show)

data UserIdGroupPair = UserIdGroupPair
    { uigpUserId :: Maybe Text
    , uigpGroupId :: Text
    , uigpGroupName :: Maybe Text
    }
  deriving (Show)

data IpRange = IpRange
    { iprCidrIp :: Text
    }
  deriving (Show)

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
  deriving (Show)

data EbsSource
    = EbsSnapshotId Text
    | EbsVolumeSize Int
  deriving (Show)

data NetworkInterfaceParam = NetworkInterfaceParam
    { nipInterfaceId :: Maybe Text
    , nipDeviceIndex :: Maybe Text
    , nipSubnetId :: Maybe Text
    , nipDescription :: Maybe Text
    , nipPrivateIpAddresses :: [Text]
    , nipSecurityGroupIds :: [Text]
    , nipDeleteOnTermination :: Maybe Bool
    }
  deriving (Show)

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
  deriving (Show)

data VpnConnectionState
    = VCSPending
    | VCSAvailable
    | VCSDeleting
    | VCSDeleted
  deriving (Show)

vpnConnectionState :: Text -> VpnConnectionState
vpnConnectionState t
    | t == "pending"   = VCSPending
    | t == "available" = VCSAvailable
    | t == "deleting"  = VCSDeleting
    | t == "deleted"   = VCSDeleted
    | otherwise        = err "vpn connection state" t

data VpnTunnelTelemetry = VpnTunnelTelemetry
    { vttOutsideIpAddress :: Text
    , vttStatus :: VpnTunnelTelemetryStatus
    , vttLastStatusChange :: UTCTime
    , vttStatusMessage :: Text
    , vttAcceptRouteCount :: Int
    }
  deriving (Show)

data VpnTunnelTelemetryStatus
    = VTTSUp
    | VTTSDown
  deriving (Show)

vpnTunnelTelemetryStatus :: Text -> VpnTunnelTelemetryStatus
vpnTunnelTelemetryStatus t
    | t == "UP"   = VTTSUp
    | t == "DOWN" = VTTSDown
    | otherwise   = err "vpn tunnel telemetry status" t

data VpnConnectionOptionsRequest = VpnConnectionOptionsRequest
    { staticRoutesOnly :: Bool
    }
  deriving (Show)

data VpnStaticRoute = VpnStaticRoute
    { vsrDestinationCidrBlock :: Text
    , vsrSource :: VpnStaticRouteSource
    , vsrState :: VpnStaticRouteState
    }
  deriving (Show)

data VpnStaticRouteSource = VSRStatic
  deriving (Show)

vpnStaticRouteSource :: Text -> VpnStaticRouteSource
vpnStaticRouteSource t
    | t == "Static" = VSRStatic
    | otherwise     = err "vpn static route source" t

data VpnStaticRouteState
    = VSRSPending
    | VSRSAvailable
    | VSRSDeleting
    | VSRSDeleted
  deriving (Show)

vpnStaticRouteState :: Text -> VpnStaticRouteState
vpnStaticRouteState t
    | t == "pending"   = VSRSPending
    | t == "available" = VSRSAvailable
    | t == "deleting"  = VSRSDeleting
    | t == "deleted"   = VSRSDeleted
    | otherwise        = err "vpn static route state" t
