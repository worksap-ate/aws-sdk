{-# LANGUAGE OverloadedStrings #-}

module AWS.EC2.Types where

import Data.Default (Default(..))
import Data.Text (Text)
import Data.Time

data EC2Response body = EC2Response
    { requestId :: Text
    , responseBody :: body
    }
  deriving (Show)

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
    , resourceValue :: Text
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
    , instanceRootDeviceName :: Text
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

resourceTag :: Text -> Text -> ResourceTag
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
    -> Text -> [InstanceBlockDeviceMapping]
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
