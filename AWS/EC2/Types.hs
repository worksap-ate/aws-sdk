module AWS.EC2.Types where

import Data.Default (Default(..))
import Data.Text (Text)
import Data.Time (UTCTime)

import AWS.Util

type Filter = (Text, [Text])

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
    , imageOwnerAlias :: Maybe Text
    , imageName :: Text
    , imageDescription :: Text
    , imageRootDeviceType :: RootDeviceType
    , imageRootDeviceName :: Maybe Text
    , blockDeviceMappings :: [BlockDeviceMapping]
    , imageVirtualizationType :: VirtualizationType
    , imageTagSet :: [ResourceTag]
    , imageHypervisor :: Hypervisor
    }
  deriving (Show)

image
    :: Text -> Text -> ImageState -> Text -> Bool
    -> [ProductCode] -> Text -> ImageType -> Maybe Text
    -> Maybe Text -> Platform -> Maybe StateReason
    -> Maybe Text -> Text -> Text -> RootDeviceType
    -> Maybe Text -> [BlockDeviceMapping] -> VirtualizationType
    -> [ResourceTag] -> Hypervisor -> Image
image i l s oid p pc a t kid rid pf
    sr oa n d rdt rdn bdms vt ts h =
    Image
        { imageId = i
        , imageLocation = l
        , imageImageState = s
        , imageOwnerId = oid
        , isPublic = p
        , imageProductCodes = pc
        , imageArchitecture = a
        , imageImageType = t
        , kernelId = kid
        , ramdiskId = rid
        , imagePlatform = pf
        , imageStateReason = sr
        , imageOwnerAlias = oa
        , imageName = n
        , imageDescription = d
        , imageRootDeviceType = rdt
        , imageRootDeviceName = rdn
        , blockDeviceMappings = bdms
        , imageVirtualizationType = vt
        , imageTagSet = ts
        , imageHypervisor = h
        }

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

productCode :: Text -> ProductCodeType -> ProductCode
productCode c t = ProductCode
    { pcCode = c
    , pcType = t
    }

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

stateReason :: Text -> Text -> StateReason
stateReason c m =
    StateReason
        { stateReasonCode = c
        , stateReasonMessage = m
        }

data RootDeviceType = EBS
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

blockDeviceMapping :: Text -> Maybe Text -> Maybe EbsBlockDevice
    -> BlockDeviceMapping
blockDeviceMapping dname v e =
    BlockDeviceMapping
        { deviceName = dname
        , virtualName = v
        , ebs = e
        }

data EbsBlockDevice = EbsBlockDevice
    { ebsSnapshotId :: Maybe Text
    , ebsVolumeSize :: Int
    , ebsDeleteOnTermination :: Bool
    , ebsVolumeType :: VolumeType
    }
  deriving (Show)

ebsBlockDevice
    :: Maybe Text -> Int -> Bool -> VolumeType
    -> EbsBlockDevice
ebsBlockDevice sid vs dot vt =
    EbsBlockDevice
        { ebsSnapshotId = sid
        , ebsVolumeSize = vs
        , ebsDeleteOnTermination = dot
        , ebsVolumeType = vt
        }

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

resourceTag :: Text -> Maybe Text -> ResourceTag
resourceTag k v =
    ResourceTag
        { resourceKey = k
        , resourceValue = v
        }

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

region :: Text -> Text -> Region
region name rep = Region
    { regionName = name
    , regionEndpoint = rep
    }

{- DescribeAvailabilityZones -}
data AvailabilityZone = AvailabilityZone
    { zoneName :: Text
    , zoneState :: Text
    , zoneRegionName :: Text
    , messageSet :: [AvailabilityZoneMessage]
    }
  deriving (Show)

type AvailabilityZoneMessage = Text

availabilityZone
    :: Text -> Text -> Text -> [AvailabilityZoneMessage]
    -> AvailabilityZone
availabilityZone name st reg msgs =
    AvailabilityZone
        { zoneName = name
        , zoneState = st
        , zoneRegionName = reg
        , messageSet = msgs
        }

{- DescribeInstances -}
data Reservation = Reservation
    { reservationId :: Text
    , ownerId :: Text
    , groupSet :: [Group]
    , instanceSet :: [Instance]
    , requesterId :: Maybe Text
    }
  deriving (Show)

reservation
    :: Text -> Text -> [Group] -> [Instance] -> Maybe Text
    -> Reservation
reservation i o g iset rid = Reservation
    { reservationId = i
    , ownerId = o
    , groupSet = g
    , instanceSet = iset
    , requesterId = rid
    }

data Instance = Instance
    { instanceId :: Text
    , instanceImageId :: Text
    , instanceState :: InstanceState
    , privateDnsName :: Text
    , dnsName :: Text
    , reason :: Text
    , keyName :: Text
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

ec2Instance
    :: Text -> Text -> InstanceState -> Text -> Text -> Text
    -> Text -> Text -> [ProductCode] -> Text -> UTCTime
    -> Placement -> Maybe Text -> Maybe Text -> Maybe Text
    -> InstanceMonitoringState -> Maybe Text -> Maybe Text
    -> Maybe Text -> Maybe Text -> Maybe Bool -> [Group]
    -> Maybe StateReason -> Architecture -> RootDeviceType
    -> Maybe Text -> [InstanceBlockDeviceMapping]
    -> InstanceLifecycle -> Maybe Text -> VirtualizationType
    -> Text -> [ResourceTag] -> Hypervisor
    -> [InstanceNetworkInterface] -> Maybe IamInstanceProfile
    -> Bool -> Instance
ec2Instance iid img istate pdns dns res kname aidx pcode
    itype ltime place kid rid pf mon snid vpcid paddr addr
    sdc grp sreason arch rdtype rdname bdmap life spotid
    vtype ctoken tset hv nicset iam eopt =
    Instance
        { instanceId = iid
        , instanceImageId = img
        , instanceState = istate
        , privateDnsName = pdns
        , dnsName = dns
        , reason = res
        , keyName = kname
        , amiLaunchIndex = aidx
        , instanceProductCodes = pcode
        , instanceType = itype
        , launchTime = ltime
        , instancePlacement = place
        , instanceKernelId = kid
        , instanceRamdiskId = rid
        , instancePlatform = pf
        , instanceMonitoring = mon
        , subnetId = snid
        , vpcId = vpcid
        , privateIpAddress = paddr
        , ipAddress = addr
        , sourceDestCheck = sdc
        , vpcGroupSet = grp
        , instanceStateReason = sreason
        , instanceArchitecture = arch
        , instanceRootDeviceType = rdtype
        , instanceRootDeviceName = rdname
        , instanceBlockDeviceMappings = bdmap
        , instanceInstanceLifecycle = life
        , spotInstanceRequestId = spotid
        , instanceVirtualizationType = vtype
        , clientToken = ctoken
        , instanceTagSet = tset
        , instanceHypervisor = hv
        , instanceNetworkInterfaceSet = nicset
        , instanceIamInstanceProfile = iam
        , ebsOptimized = eopt
        }

data InstanceStatus = InstanceStatus
    { isInstanceId :: Text
    , isAvailabilityZone :: Text
    , isEventsSet :: [InstanceStatusEvent]
    , isInstanceState :: InstanceState
    , isSystemStatus :: InstanceStatusType
    , isInstanceStatus :: InstanceStatusType
    }
  deriving (Show)

instanceStatus :: Text -> Text -> [InstanceStatusEvent]
    -> InstanceState -> InstanceStatusType
    -> InstanceStatusType -> InstanceStatus
instanceStatus iid az es ist sst iss = InstanceStatus
    { isInstanceId = iid
    , isAvailabilityZone = az
    , isEventsSet = es
    , isInstanceState = ist
    , isSystemStatus = sst
    , isInstanceStatus = iss
    }

data InstanceStatusEvent = InstanceStatusEvent
    { iseCode :: InstanceStatusEventCode
    , iseDescription :: Text
    , iseNotBefore :: Maybe UTCTime
    , iseNotAfter :: Maybe UTCTime
    }
  deriving (Show)

instanceStatusEvent :: InstanceStatusEventCode
    -> Text -> Maybe UTCTime -> Maybe UTCTime
    -> InstanceStatusEvent
instanceStatusEvent code desc before after = InstanceStatusEvent
    { iseCode = code
    , iseDescription = desc
    , iseNotBefore = before
    , iseNotAfter = after
    }

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

instanceStatusType :: InstanceStatusTypeStatus
    -> [InstanceStatusDetail] -> InstanceStatusType
instanceStatusType status details = InstanceStatusType
    { isdStatus = status
    , isdDetails = details
    }

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

instanceStatusDetail :: InstanceStatusDetailName
    -> InstanceStatusDetailStatus -> Maybe UTCTime
    -> InstanceStatusDetail
instanceStatusDetail name status since =
    InstanceStatusDetail
        { isddName = name
        , isddStatus = status
        , isddImpairedSince = since
        }

type InstanceStatusDetailName = Text

type InstanceStatusDetailStatus = Text

data Group = Group
    { groupId :: Text
    , groupName :: Text
    }
  deriving (Show)

group :: Text -> Text -> Group
group i n = Group
    { groupId = i
    , groupName = n
    }

data InstanceState
    = Pending
    | Running
    | ShuttingDown
    | Terminated
    | Stopping
    | Stopped
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
    Nothing -> error "invalid state code"
    Just st -> st

data Placement = Placement
    { placementAvailabilityZone :: Text
    , placementGroupName :: Text
    , tenancy :: Text
    }
  deriving (Show)

placement :: Text -> Text -> Text -> Placement
placement zone gname ten = Placement
    { placementAvailabilityZone = zone
    , placementGroupName = gname
    , tenancy = ten
    }

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

instanceBlockDeviceMapping
    :: Text -> InstanceEbsBlockDevice
    -> InstanceBlockDeviceMapping
instanceBlockDeviceMapping devname iebs =
    InstanceBlockDeviceMapping
        { instanceDeviceName = devname
        , instanceEbs = iebs
        }

data InstanceEbsBlockDevice = InstanceEbsBlockDevice
    { instanceEbsVolumeId :: Text
    , instanceEbsState :: VolumeState
    , instanceEbsAttachTime :: UTCTime
    , instanceEbsDeleteOnTermination :: Bool
    }
  deriving (Show)

instanceEbsBlockDevice
    :: Text -> VolumeState -> UTCTime -> Bool
    -> InstanceEbsBlockDevice
instanceEbsBlockDevice vid vst atime dot =
    InstanceEbsBlockDevice
        { instanceEbsVolumeId = vid
        , instanceEbsState = vst
        , instanceEbsAttachTime = atime
        , instanceEbsDeleteOnTermination = dot
        }

data VolumeState
    = VolumeAttaching
    | VolumeAttached
    | VolumeDetaching
    | VolumeDetached
  deriving (Show)

volumeState :: Text -> VolumeState
volumeState t
    | t == "attached"  = VolumeAttached
    | t == "attaching" = VolumeAttaching
    | t == "detaching" = VolumeDetaching
    | t == "detached"  = VolumeDetached
    | otherwise        = err "volume state" t

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

instanceNetworkInterface
    :: Text -> Text -> Text -> Text -> Text -> Text -> Text
    -> Maybe Text -> Bool -> [Group] -> NetworkInterfaceAttachment
    -> Maybe NetworkInterfaceAssociation
    -> [InstancePrivateIpAddress]
    -> InstanceNetworkInterface
instanceNetworkInterface
    iid sid vpcid desc own st paddr pdns sdc grp att asso pips =
    InstanceNetworkInterface
        { instanceNetworkInterfaceId = iid
        , iniSubnetId = sid
        , iniVpcId = vpcid
        , iniDescription = desc
        , iniOwnerId = own
        , iniStatus = st
        , iniPrivateIpAddress = paddr
        , iniPrivateDnsName = pdns
        , iniSourceDestCheck = sdc
        , iniGroupSet = grp
        , iniAttachment = att
        , iniAssociation = asso
        , iniPrivateIpAddressSet = pips
        }

data NetworkInterfaceAttachment = NetworkInterfaceAttachment
    { niatAttachmentId :: Text
    , niatDeviceIndex :: Int
    , niatStatus :: Text
    , niatAttachTime :: UTCTime
    , niatDeleteOnTermination :: Bool
    }
  deriving (Show)

networkInterfaceAttachment
    :: Text -> Int -> Text -> UTCTime -> Bool
    -> NetworkInterfaceAttachment
networkInterfaceAttachment aid idx st time dot =
    NetworkInterfaceAttachment
        { niatAttachmentId = aid
        , niatDeviceIndex = idx
        , niatStatus = st
        , niatAttachTime = time
        , niatDeleteOnTermination = dot
        }

data NetworkInterfaceAssociation = NetworkInterfaceAssociation
    { niasPublicIp :: Text
    , niasIpOwnerId :: Text
    }
  deriving (Show)

networkInterfaceAssociation
    :: Text -> Text -> NetworkInterfaceAssociation
networkInterfaceAssociation ip own =
    NetworkInterfaceAssociation
        { niasPublicIp = ip
        , niasIpOwnerId = own
        }

data InstancePrivateIpAddress = InstancePrivateIpAddress
    { iPrivateIpAddress :: Text
    , iPrimary :: Bool
    , iAssociation :: Maybe NetworkInterfaceAssociation
    }
  deriving (Show)

instancePrivateIpAddress
    :: Text -> Bool -> Maybe NetworkInterfaceAssociation
    -> InstancePrivateIpAddress
instancePrivateIpAddress ip pr asso =
    InstancePrivateIpAddress
        { iPrivateIpAddress = ip
        , iPrimary = pr
        , iAssociation = asso
        }

data IamInstanceProfile = IamInstanceProfile
    { iipArn :: Text
    , iipId :: Text
    }
  deriving (Show)

iamInstanceProfile :: Text -> Text -> IamInstanceProfile
iamInstanceProfile arn iid = IamInstanceProfile
    { iipArn = arn
    , iipId = iid
    }

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

address :: Text -> Maybe Text -> AddressDomain -> Maybe Text
    -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text
    -> Address
address pip alid dom iid asid niid nioid pips = Address
    { addrPublicIp = pip
    , addrAllocationId = alid
    , addrDomain = dom
    , addrInstanceId = iid
    , addrAssociationId = asid
    , addrNetworkInterfaceId = niid
    , addrNetworkInterfaceOwnerId = nioid
    , addrPrivateIpAddress = pips
    }

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

allocateAddressResponse :: Text -> AddressDomain -> Maybe Text
    -> AllocateAddressResponse
allocateAddressResponse ip domain allid =
    AllocateAddressResponse
        { alaPublicIp = ip
        , alaDomain = domain
        , alaAllocationId = allid
        }

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

tag :: Text -> Text -> Text -> Maybe Text -> Tag
tag tid ttype key value = Tag
    { tagResourceId = tid
    , tagResourceType = ttype
    , tagKey = key
    , tagValue = value
    }
