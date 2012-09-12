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
    , hipervisor :: Hipervisor
    }
  deriving (Show)

data ImageState
    = ImageAvailable
    | ImagePending
    | ImageFailed
  deriving (Show)

data ProductCode = ProductCode
    { productCode :: Text
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

data Hipervisor = OVM
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
    , instanceHypervisor :: Hipervisor
    , instanceNetworkInterfaceSet :: [InstanceNetworkInterface]
    , iamInstanceProfile :: Maybe IamInstanceProfile
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
    { availabilityZone :: Text
    , placementGroupName :: Text
    , tenancy :: Text
    }
  deriving (Show)

data InstanceMonitoringState
    = MonitoringDisabled
    | MonitoringEnable
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

data InstanceLifecycle = LifeCycleSpot | LifeCycleNone
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

