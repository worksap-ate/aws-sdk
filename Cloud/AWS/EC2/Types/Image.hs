{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.EC2.Types.Image
    ( AMIAttribute(..)
    , AMIAttributeDescription(..)
    , BlockDeviceMapping(..)
    , BlockDeviceMappingParam(..)
    , EbsBlockDevice(..)
    , EbsSource(..)
    , Image(..)
    , ImageState(..)
    , ImageType(..)
    , LaunchPermission(..)
    , LaunchPermissionItem(..)
    , ProductCodeItem(..)
    , RegisterImageRequest(..)
    ) where

import Cloud.AWS.EC2.Types.Common
import Cloud.AWS.EC2.Types.Volume (VolumeType)
import Cloud.AWS.Lib.FromText (deriveFromText)
import Cloud.AWS.Lib.ToText (deriveToText)
import Data.Text (Text)

data AMIAttribute
    = AMIDescription
    | AMIKernel
    | AMIRamdisk
    | AMILaunchPermission
    | AMIProductCodes
    | AMIBlockDeviceMapping
  deriving (Show, Read, Eq)

deriveToText "AMIAttribute"
    [ "description"
    , "kernel"
    , "ramdisk"
    , "launchPermission"
    , "productCodes"
    , "blockDeviceMapping"
    ]

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

data BlockDeviceMapping = BlockDeviceMapping
    { blockDeviceMappingDeviceName :: Text
    , blockDeviceMappingVirtualName :: Maybe Text
    , blockDeviceMappingEbs :: Maybe EbsBlockDevice
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

data EbsBlockDevice = EbsBlockDevice
    { ebsSnapshotId :: Maybe Text
    , ebsVolumeSize :: Maybe Int
    , ebsDeleteOnTermination :: Bool
    , ebsVolumeType :: VolumeType
    }
  deriving (Show, Read, Eq)

data EbsSource
    = EbsSourceSnapshotId Text
    | EbsSourceVolumeSize Int
  deriving (Show, Read, Eq)

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

data ImageState
    = ImageStateAvailable
    | ImageStatePending
    | ImageStateFailed
  deriving (Show, Read, Eq)

data ImageType
    = ImageTypeMachine
    | ImageTypeKernel
    | ImageTypeRamDisk
  deriving (Show, Read, Eq)

data LaunchPermission = LaunchPermission
    { launchPermissionAdd :: [LaunchPermissionItem]
    , launchPermissionRemove :: [LaunchPermissionItem]
    }
  deriving (Show, Read, Eq)

data LaunchPermissionItem
    = LaunchPermissionItemGroup Text
    | LaunchPermissionItemUserId Text
  deriving (Show, Read, Eq)

data ProductCodeItem = ProductCodeItem
    { productCodeItemProductCode :: Text
    }
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

deriveFromText "ImageState" ["available", "pending", "failed"]
deriveFromText "ImageType" ["machine", "kernel", "ramdisk"]
