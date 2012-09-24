module AWS.EC2.Types where

import Data.Default (Default(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Conduit as HTTP
import Data.Text.Read (decimal)
import Safe (readMay)
import System.Locale (defaultTimeLocale)
import Data.Time (UTCTime, readTime)

import AWS.Types

data EC2Context = EC2Context
    { manager :: HTTP.Manager
    , credential :: Credential
    , endpoint :: EC2Endpoint
    , lastRequestId :: Maybe Text
    }

data QueryParam
    = ArrayParams Text [Text]
    | FilterParams [Filter]
    | ValueParam Text Text
  deriving (Show)

type Filter = (Text, [Text])

data Image = Image
    { imageId :: Text
    , imageLocation :: Text
    , imageState :: ImageState
    , imageOwnerId :: Text
    , isPublic :: Bool
    , imageProductCodes :: [ProductCode]
    , imageArchitecture :: Text
    , imageType :: ImageType
    , kernelId :: Maybe Text
    , ramdiskId :: Maybe Text
    , platform :: Platform
    , imageStateReason :: Maybe StateReason
    , imageOwnerAlias :: Maybe Text
    , imageName :: Text
    , description :: Text
    , rootDeviceType :: RootDeviceType
    , rootDeviceName :: Maybe Text
    , blockDeviceMappings :: [BlockDeviceMapping]
    , virtualizationType :: VirtualizationType
    , imageTagSet :: [ResourceTag]
    , hypervisor :: Hypervisor
    }
  deriving (Show)

data ImageState
    = ImageAvailable
    | ImagePending
    | ImageFailed
  deriving (Show)

data ProductCode = ProductCode
    { productCodeCode :: Text
    , productCodeType :: ProductCodeType
    }
  deriving (Show)

data ProductCodeType = Devpay
                     | Marketplace
  deriving (Show)

data ImageType = Machine
               | Kernel
               | RamDisk
  deriving (Show)

data Platform = Windows
              | Other
  deriving (Show)

data StateReason = StateReason
    { stateReasonCode :: Text
    , stateReasonMessage :: Text
    }
  deriving (Show)

data RootDeviceType = EBS
                    | InstanceStore
  deriving (Show)

data BlockDeviceMapping = BlockDeviceMapping
    { deviceName :: Text
    , virtualName :: Maybe Text
    , ebs :: Maybe EbsBlockDevice
    }
  deriving (Show)

data EbsBlockDevice = EbsBlockDevice
    { ebsSnapshotId :: Maybe Text
    , volumeSize :: Int
    , ebsDeleteOnTermination :: Bool
    , volumeType :: VolumeType
    , iops :: Maybe Int
    }
  deriving (Show)

data VolumeType = Standard
                | IO1
  deriving (Show)

instance Default VolumeType
  where
    def = Standard

data VirtualizationType = Paravirtual
                        | HVM
  deriving (Show)

data ResourceTag = ResourceTag
    { resourceKey :: Text
    , resourceValue :: Maybe Text
    }
  deriving (Show)

data Hypervisor = OVM
                | Xen
  deriving (Show)

{- DescribeRegions -}
data Region = Region
    { regionName :: Text
    , regionEndpoint :: Text
    }
  deriving (Show)

{- DescribeAvailabilityZones -}
data AvailabilityZone = AvailabilityZone
    { zoneName :: Text
    , zoneState :: Text
    , zoneRegionName :: Text
    , messageSet :: [AvailabilityZoneMessage]
    }
  deriving (Show)

type AvailabilityZoneMessage = Text

{- DescribeInstances -}
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
    , keyName :: Text
    , amiLaunchIndex :: Text
    , instanceProductCodes :: [ProductCode]
    , instanceType :: Text
    , launchTime :: UTCTime
    , instancePlacement :: Placement
    , instanceKernelId :: Maybe Text
    , instanceRamdiskId :: Maybe Text
    , instancePlatform :: Maybe Text
    , monitoring :: InstanceMonitoringState
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
    , instanceLifecycle :: InstanceLifecycle
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

data InstanceMonitoringState
    = MonitoringDisabled
    | MonitoringEnabled
    | MonitoringPending
  deriving (Show)

data Architecture = I386 | X86_64 deriving (Show)

data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping
    { instanceDeviceName :: Text
    , instanceEbs :: InstanceEbsBlockDevice
    }
  deriving (Show)

data InstanceEbsBlockDevice = InstanceEbsBlockDevice
    { instanceEbsVolumeId :: Text
    , instanceEbsState :: VolumeState
    , instanceEbsAttachTime :: UTCTime
    , instanceEbsDeleteOnTermination :: Bool
    }
  deriving (Show)

data VolumeState
    = VolumeAttaching
    | VolumeAttached
    | VolumeDetaching
    | VolumeDetached
  deriving (Show)

data InstanceLifecycle = LifecycleSpot | LifecycleNone
  deriving (Show)

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

blockDeviceMapping :: Text -> Maybe Text -> Maybe EbsBlockDevice
    -> BlockDeviceMapping
blockDeviceMapping dname v e =
    BlockDeviceMapping
        { deviceName = dname
        , virtualName = v
        , ebs = e
        }

ebsBlockDevice
    :: Maybe Text -> Int -> Bool -> VolumeType -> Maybe Int
    -> EbsBlockDevice
ebsBlockDevice sid vs dot vt io =
    EbsBlockDevice
        { ebsSnapshotId = sid
        , volumeSize = vs
        , ebsDeleteOnTermination = dot
        , volumeType = vt
        , iops = io
        }

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
        , imageState = s
        , imageOwnerId = oid
        , isPublic = p
        , imageProductCodes = pc
        , imageArchitecture = a
        , imageType = t
        , kernelId = kid
        , ramdiskId = rid
        , platform = pf
        , imageStateReason = sr
        , imageOwnerAlias = oa
        , imageName = n
        , description = d
        , rootDeviceType = rdt
        , rootDeviceName = rdn
        , blockDeviceMappings = bdms
        , virtualizationType = vt
        , imageTagSet = ts
        , hypervisor = h
        }

resourceTag :: Text -> Maybe Text -> ResourceTag
resourceTag k v =
    ResourceTag
        { resourceKey = k
        , resourceValue = v
        }

productCode :: Text -> ProductCodeType -> ProductCode
productCode c t =
    ProductCode
        { productCodeCode = c
        , productCodeType = t
        }

stateReason :: Text -> Text -> StateReason
stateReason c m =
    StateReason
        { stateReasonCode = c
        , stateReasonMessage = m
        }

region :: Text -> Text -> Region
region name rep = Region
    { regionName = name
    , regionEndpoint = rep
    }

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

group :: Text -> Text -> Group
group i n = Group
    { groupId = i
    , groupName = n
    }

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
        , monitoring = mon
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
        , instanceLifecycle = life
        , spotInstanceRequestId = spotid
        , instanceVirtualizationType = vtype
        , clientToken = ctoken
        , instanceTagSet = tset
        , instanceHypervisor = hv
        , instanceNetworkInterfaceSet = nicset
        , instanceIamInstanceProfile = iam
        , ebsOptimized = eopt
        }

placement
    :: Text -> Text -> Text -> Placement
placement zone gname ten =
    Placement
        { placementAvailabilityZone = zone
        , placementGroupName = gname
        , tenancy = ten
        }

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

instanceBlockDeviceMapping
    :: Text -> InstanceEbsBlockDevice
    -> InstanceBlockDeviceMapping
instanceBlockDeviceMapping devname iebs =
    InstanceBlockDeviceMapping
        { instanceDeviceName = devname
        , instanceEbs = iebs
        }

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
        
iamInstanceProfile :: Text -> Text -> IamInstanceProfile
iamInstanceProfile arn iid = IamInstanceProfile
    { iipArn = arn
    , iipId = iid
    }

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

instancePrivateIpAddress
    :: Text -> Bool -> Maybe NetworkInterfaceAssociation
    -> InstancePrivateIpAddress
instancePrivateIpAddress ip pr asso =
    InstancePrivateIpAddress
        { iPrivateIpAddress = ip
        , iPrimary = pr
        , iAssociation = asso
        }

networkInterfaceAssociation
    :: Text -> Text -> NetworkInterfaceAssociation
networkInterfaceAssociation ip own =
    NetworkInterfaceAssociation
        { niasPublicIp = ip
        , niasIpOwnerId = own
        }

err :: String -> Text -> a
err v m = error $ "unknown " ++ v ++ ": " ++ T.unpack m

t2imageState :: Text -> ImageState
t2imageState a
    | a == "available" = ImageAvailable
    | a == "pending"   = ImagePending
    | a == "failed"    = ImageFailed
    | otherwise        = err "image state" a

t2bool :: Text -> Bool
t2bool a
    | a == "true"  = True
    | a == "false" = False
    | otherwise    = err "value" a

t2dec :: Integral a => Text -> a
t2dec t = either 
    (const $ error "not decimal")
    fst
    (decimal t)

t2imageType :: Text -> ImageType
t2imageType t
    | t == "machine"  = Machine
    | t == "kernel"   = Kernel
    | t == "ramdisk" = RamDisk
    | otherwise       = err "image type" t

t2platform :: Maybe Text -> Platform
t2platform Nothing   = Other
t2platform (Just t)
    | t == "windows" = Windows
    | otherwise      = Other

t2rootDeviceType :: Text -> RootDeviceType
t2rootDeviceType t
    | t == "ebs"            = EBS
    | t == "instance-store" = InstanceStore
    | otherwise             = err "root device type" t

t2virtualizationType :: Text -> VirtualizationType
t2virtualizationType t
    | t == "paravirtual" = Paravirtual
    | t == "hvm"         = HVM
    | otherwise          = err "virtualization type" t

t2hypervisor :: Text -> Hypervisor
t2hypervisor t
    | t == "xen" = Xen
    | t == "ovm" = OVM
    | otherwise  = err "hypervisor" t

t2volumeType :: Text -> VolumeType
t2volumeType t
    | t == "standard" = Standard
    | t == "io1"      = IO1
    | otherwise       = err "volume type" t

t2iops :: Maybe Text -> Maybe Int
t2iops mt = mt >>= readMay . T.unpack

t2productCodeType :: Text -> ProductCodeType
t2productCodeType t
    | t == "marketplace" = Marketplace
    | t == "devpay"      = Devpay
    | otherwise          = err "product code type" t

t2time :: Text -> UTCTime
t2time = readTime defaultTimeLocale fmt . T.unpack
  where
    fmt = "%FT%T.000Z"

t2emptxt :: Maybe Text -> Text
t2emptxt = maybe "" id

t2volumeState :: Text -> VolumeState
t2volumeState t
    | t == "attached"  = VolumeAttached
    | t == "attaching" = VolumeAttaching
    | t == "detaching" = VolumeDetaching
    | t == "detached"  = VolumeDetached
    | otherwise        = err "volume state" t

t2architecture :: Text -> Architecture
t2architecture t
    | t == "i386"   = I386
    | t == "x86_64" = X86_64
    | otherwise     = err "architecture" t

t2deviceType :: Text -> RootDeviceType
t2deviceType t
    | t == "ebs"            = EBS
    | t == "instance-store" = InstanceStore
    | otherwise             = err "root device type" t

t2monitoring :: Text -> InstanceMonitoringState
t2monitoring t
    | t == "disabled" = MonitoringDisabled
    | t == "enabled"  = MonitoringEnabled
    | t == "pending"  = MonitoringPending
    | otherwise       = err "monitoring state" t

t2lifecycle :: Maybe Text -> InstanceLifecycle
t2lifecycle Nothing = LifecycleNone
t2lifecycle (Just t)
    | t == "spot"   = LifecycleSpot
    | otherwise     = err "lifecycle" t

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
