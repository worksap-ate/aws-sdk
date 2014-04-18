{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.EC2.Types.ConversionTask
    ( ConversionTask(..)
    , ConversionTaskState(..)
    , DiskImage(..)
    , DiskImageDescription(..)
    , DiskImageVolumeDescription(..)
    , ImportInstanceTaskDetailItem(..)
    , ImportInstanceTaskDetails(..)
    , ImportVolumeRequestImage(..)
    , ImportVolumeTaskDetails(..)
    , LaunchSpecification(..)
    ) where

import Cloud.AWS.EC2.Types.Common (Architecture, ShutdownBehavior)
import Cloud.AWS.Lib.FromText (deriveFromText)
import Data.Text (Text)
import Data.IP (IPv4)

data ConversionTask = ConversionTask
    { conversionTaskId :: Text
    , conversionTaskExpirationTime :: Text
    , conversionTaskImportVolume :: Maybe ImportVolumeTaskDetails
    , conversionTaskImportInstance :: Maybe ImportInstanceTaskDetails
    , conversionTaskState :: ConversionTaskState
    , conversionTaskStatusMessage :: Text
    }
  deriving (Show, Read, Eq)

data ConversionTaskState
    = ConversionTaskActive
    | ConversionTaskCancelling
    | ConversionTaskCancelled
    | ConversionTaskCompleted
  deriving (Show, Read, Eq)

data DiskImage = DiskImage
    { diskImageFormat :: Text
    , diskImageBytes :: Int
    , diskImageImportManifestUrl :: Text
    , diskImageDescripsion :: Maybe Text
    , diskImageVolumeSize :: Int
    }
  deriving (Show, Read, Eq)

data DiskImageDescription = DiskImageDescription
    { diskImageDescriptionFormat :: Text
    , diskImageDescriptionSize :: Int
    , diskImageDescriptionImportManifestUrl :: Text
    , diskImageDescriptionChecksum :: Maybe Text
    }
  deriving (Show, Read, Eq)

data DiskImageVolumeDescription = DiskImageVolumeDescription
    { diskImageVolumeDescriptionSize :: Int
    , diskImageVolumeDescriptionId :: Maybe Text
    }
  deriving (Show, Read, Eq)

data ImportInstanceTaskDetailItem = ImportInstanceTaskDetailItem
    { importInstanceTaskDetailItemBytesConverted :: Int
    , importInstanceTaskDetailItemAvailabilityZone :: Text
    , importInstanceTaskDetailItemImage :: DiskImageDescription
    , importInstanceTaskDetailItemDescription :: Maybe Text
    , importInstanceTaskDetailItemVolume :: DiskImageVolumeDescription
    , importInstanceTaskDetailItemStatus :: Text
    , importInstanceTaskDetailItemStatusMessage :: Maybe Text
    }
  deriving (Show, Read, Eq)

data ImportInstanceTaskDetails = ImportInstanceTaskDetails
    { importInstanceTaskDetailsVolumes :: [ImportInstanceTaskDetailItem]
    , importInstanceTaskDetailsInstanceId :: Text
    , importInstanceTaskDetailsPlatform :: Maybe Text
    , importInstanceTaskDetailsDescription :: Maybe Text
    }
  deriving (Show, Read, Eq)

data ImportVolumeRequestImage = ImportVolumeRequestImage
    { importVolumeRequestImageFormat :: Text
    , importVolumeRequestImageBytes :: Int
    , importVolumeRequestImageImportManifestUrl :: Text
    }
  deriving (Show, Read, Eq)

data ImportVolumeTaskDetails = ImportVolumeTaskDetails
    { importVolumeTaskDetailsBytesConverted :: Int
    , importVolumeTaskDetailsAvailabilityZone :: Text
    , importVolumeTaskDetailsDescription :: Maybe Text
    , importVolumeTaskDetailsImage :: DiskImageDescription
    , importVolumeTaskDetailsVolume :: DiskImageVolumeDescription
    }
  deriving (Show, Read, Eq)

data LaunchSpecification = LaunchSpecification
    { launchSpecificationArchitecture :: Architecture
    , launchSpecificationGroupNames :: [Text]
    , launchSpecificationUserData :: Maybe Text
    , launchSpecificationInstanceType :: Text
    , launchSpecificationPlacementAvailabilityZone :: Maybe Text
    , launchSpecificationMonitoringEnabled :: Maybe Bool
    , launchSpecificationSubnetId :: Maybe Text
    , launchSpecificationInstanceInitiatedShutdownBehavior
        :: Maybe ShutdownBehavior
    , launchSpecificationPrivateIpAddress :: Maybe IPv4
    }
  deriving (Show, Read, Eq)

deriveFromText "ConversionTaskState"
    ["active", "cancelling", "cancelled", "completed"]
