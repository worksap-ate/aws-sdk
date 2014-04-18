{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.EC2.Types.Volume
    ( AttachmentSetItemResponse(..)
    , AttachmentSetItemResponseStatus(..)
    , CreateVolumeRequest(..)
    , Volume(..)
    , VolumeAttribute(..)
    , VolumeAttributeRequest(..)
    , VolumeState(..)
    , VolumeStatus(..)
    , VolumeStatusAction(..)
    , VolumeStatusDetail(..)
    , VolumeStatusEvent(..)
    , VolumeStatusInfo(..)
    , VolumeStatusInfoStatus(..)
    , VolumeType(..)
    ) where

import Cloud.AWS.EC2.Types.Common (ProductCode, ResourceTag)
import Cloud.AWS.Lib.FromText (deriveFromText)
import Cloud.AWS.Lib.ToText (deriveToText)
import Data.Text (Text)
import Data.Time (UTCTime)

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

data VolumeAttribute
    = VolumeAttributeAutoEnableIO Bool
    | VolumeAttributeProductCodes [ProductCode]
  deriving (Show, Read, Eq)

data VolumeAttributeRequest
    = VolumeAttributeRequestAutoEnableIO
    | VolumeAttributeRequestProductCodes
  deriving (Show, Read, Eq)

data VolumeState
    = VolumeStateCreating
    | VolumeStateAvailable
    | VolumeStateInUse
    | VolumeStateDeleting
    | VolumeStateDeleted
    | VolumeStateError
  deriving (Show, Read, Eq)

data VolumeStatus = VolumeStatus
    { volumeStatusVolumeId :: Text
    , volumeStatusAvailabilityZone :: Text
    , volumeStatusVolumeStatus :: VolumeStatusInfo
    , volumeStatusEventSet :: [VolumeStatusEvent]
    , volumeStatusActionSet :: [VolumeStatusAction]
    }
  deriving (Show, Read, Eq)

data VolumeStatusAction = VolumeStatusAction
    { volumeStatusActionCode :: Text
    , volumeStatusActionEventType :: Text
    , volumeStatusActionEventId :: Text
    , volumeStatusActionDescription :: Text
    }
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

data VolumeType
    = VolumeTypeStandard
    | VolumeTypeIO1 Int
  deriving (Show, Read, Eq)

deriveFromText "AttachmentSetItemResponseStatus"
    ["attaching", "attached", "detaching", "detached"]
deriveFromText "VolumeState"
    [ "creating"
    , "available"
    , "in-use"
    , "deleting"
    , "deleted"
    , "error"
    ]
deriveFromText "VolumeStatusInfoStatus"
    ["ok", "impaired", "insufficient-data"]

deriveToText "VolumeAttributeRequest"
    [ "autoEnableIO", "productCodes" ]
