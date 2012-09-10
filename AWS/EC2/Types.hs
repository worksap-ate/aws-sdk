{-# LANGUAGE OverloadedStrings #-}

module AWS.EC2.Types where

import Data.Default (Default(..))
import Data.Text (Text)

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
    , productCodes :: [ProductCode]
    , architecture :: Text
    , imageType :: ImageType
    , kernelId :: Maybe Text
    , ramdiskId :: Maybe Text
    , platform :: Platform
    , stateReason :: Maybe StateReason
    , imageOwnerAlias :: Maybe Text
    , imageName :: Maybe Text
    , description :: Maybe Text
    , rootDeviceType :: RootDeviceType
    , rootDeviceName :: Maybe Text
    , blockDeviceMappings :: [BlockDeviceMapping]
    , virtualizationType :: VirtualizationType
    , tagSet :: [ResourceTag]
    , hipervisor :: Hipervisor
    }
  deriving (Show)

data ImageState = Available
                | Pending
                | Failed
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
    { snapshotId :: Maybe Text
    , volumeSize :: Int
    , deleteOnTermination :: Bool
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

