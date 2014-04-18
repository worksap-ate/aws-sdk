{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.EC2.Types.SpotInstance 
    ( SpotInstanceRequest (..)
    , SpotInstanceType (..)
    , SpotInstanceState (..)
    , SpotInstanceStatus (..)
    , SpotInstanceFault (..)
    , SpotInstanceLaunchSpecification (..)
    , SpotInstanceBlockDeviceMapping (..)
    , EbsSpotInstanceBlockDevice (..)
    , SpotInstanceMonitoringState (..)
    , SpotInstanceNetworkInterface (..)
    , SpotInstanceSecurityGroup (..)
    , RequestSpotInstancesParam (..)
    , CancelSpotInstanceRequestsResponse (..)
    ) where

import Cloud.AWS.EC2.Types.Common
import Cloud.AWS.EC2.Types.Instance
import Cloud.AWS.EC2.Types.Image (BlockDeviceMappingParam)
import Cloud.AWS.EC2.Types.NetworkInterface (NetworkInterfaceParam)

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Cloud.AWS.Lib.FromText (deriveFromText)

data SpotInstanceRequest = SpotInstanceRequest
    { spotInstanceRequestId :: Text
    , spotInstanceRequestSpotPrice :: Text
    , spotInstanceRequestType :: SpotInstanceType
    , spotInstanceRequestState :: SpotInstanceState
    , spotInstanceRequestFault :: Maybe SpotInstanceFault
    , spotInstanceRequestStatus :: SpotInstanceStatus
    , spotInstanceRequestValidFrom :: Maybe UTCTime
    , spotInstanceRequestValidUntil :: Maybe UTCTime
    , spotInstanceRequestLaunchGroup :: Maybe Text
    , spotInstanceRequestAvailabilityZoneGroup :: Maybe Text
    , spotInstanceRequestLaunchSpecification :: SpotInstanceLaunchSpecification
    , spotInstanceRequestInstanceId :: Maybe Text
    , spotInstanceRequestCreateTime :: UTCTime
    , spotInstanceRequestProductDescription :: Maybe Text
    , spotInstanceRequestTagSet :: [ResourceTag]
    , spotInstanceRequestLaunchedAvailabilityZone :: Maybe Text
    }
  deriving (Show, Read, Eq)

data SpotInstanceType = SpotInstanceTypeOneTime 
    | SpotInstanceTypePersistent deriving (Show, Read, Eq)

data SpotInstanceState = SpotInstanceStateOpen 
    | SpotInstanceStateActive
    | SpotInstanceStateClosed
    | SpotInstanceStateCancelled
    | SpotInstanceStateFailed
  deriving (Show, Read, Eq)

data SpotInstanceStatus = SpotInstanceStatus
    { spotInstanceStatusCode :: Text
    , spotInstanceStatusUpdateTime :: UTCTime
    , spotInstanceStatusMessage :: Maybe Text
    }
  deriving (Show, Read, Eq)

data SpotInstanceFault = SpotInstanceFault
    { spotInstanceFaultCode :: Text
    , spotInstanceFaultMessage :: Maybe Text
    }
  deriving (Show, Read, Eq)

data SpotInstanceLaunchSpecification = SpotInstanceLaunchSpecification
    { spotInstanceLaunchSpecificationImageId :: Text
    , spotInstanceLaunchSpecificationKeyName :: Maybe Text
    , spotInstanceLaunchSpecificationvpcGroupSet :: [Group]
    , spotInstanceLaunchSpecificationInstanceType :: Text
    , spotInstanceLaunchSpecificationPlacement :: Placement
    , spotInstanceLaunchSpecificationKernelId :: Maybe Text
    , spotInstanceLaunchSpecificationRamdiskId :: Maybe Text
    , spotInstanceLaunchSpecificationBlockDeviceMappings :: [SpotInstanceBlockDeviceMapping]
    , spotInstanceLaunchSpecificationMonitoring :: SpotInstanceMonitoringState
    , spotInstanceLaunchSpecificationSubnetId :: Maybe Text
    , spotInstanceLaunchSpecificationNetworkInterfaceSet :: [SpotInstanceNetworkInterface]
    , spotInstanceLaunchSpecificationIamInstanceProfile :: Maybe IamInstanceProfile
    , spotInstanceLaunchSpecificationEbsOptimized :: Maybe Bool -- default: false
    }
  deriving (Show, Read, Eq)

data SpotInstanceBlockDeviceMapping = SpotInstanceBlockDeviceMapping
    { spotInstanceBlockDeviceMappingDeviceName :: Text
    , spotInstanceBlockDeviceMappingEbs :: EbsSpotInstanceBlockDevice
    }
  deriving (Show, Read, Eq)

data EbsSpotInstanceBlockDevice = EbsSpotInstanceBlockDevice
    { ebsSpotInstanceBlockDeviceVolumeSize :: Text
    , ebsSpotInstanceBlockDeviceDeleteOnTermination :: Bool
    , ebsSpotInstanceBlockDeviceVolumeType :: Text
    }
  deriving (Show, Read, Eq)

data SpotInstanceMonitoringState = SpotInstanceMonitoringState
    { spotInstanceMonitoringStateEnabled :: Bool
    }
  deriving (Show, Read, Eq)

data SpotInstanceNetworkInterface = SpotInstanceNetworkInterface
    { spotInstanceNetworkInterfaceDeviceIndex :: Text
    , spotInstanceNetworkInterfaceSubnetId :: Text
    , spotInstanceNetworkInterfaceGroupSet :: [SpotInstanceSecurityGroup]
    }
  deriving (Show, Read, Eq)

data SpotInstanceSecurityGroup = SpotInstanceSecurityGroup
    { securtyGroupId :: Text
    }
  deriving (Show, Read, Eq)

data RequestSpotInstancesParam = RequestSpotInstancesParam
    { requestSpotInstancesSpotPrice :: Text -- ^ Required
    , requestSpotInstancesCount :: Maybe Int 
    , requestSpotInstancesType :: Maybe Text
    , requestSpotInstancesValidFrom :: Maybe UTCTime
    , requestSpotInstancesValidUntil :: Maybe UTCTime
    , requestSpotInstancesLaunchGroup :: Maybe Text
    , requestSpotInstancesAvailabilityZoneGroup :: Maybe Text
    , requestSpotInstancesImageId :: Text -- ^ Required
    , requestSpotInstancesKeyName :: Maybe Text
    , requestSpotInstancesSecurityGroupIds :: [Text]
      -- ^ SecurityGroupIds (Required for VPC; optional for EC2)
    , requestSpotInstancesSecurityGroups :: [Text]
      -- ^ SecurityGroups (Only for EC2; either id or name is accepted)
    , requestSpotInstancesUserData :: Maybe ByteString
      -- ^ UserData (Base64-encoded MIME user data)
    , requestSpotInstancesInstanceType :: Text
    , requestSpotInstancesAvailabilityZone :: Maybe Text
    , requestSpotInstancesPlacementGroup :: Maybe Text
    , requestSpotInstancesKernelId :: Maybe Text
    , requestSpotInstancesRamdiskId :: Maybe Text
    , requestSpotInstancesBlockDeviceMappings
        :: [BlockDeviceMappingParam]
    , requestSpotInstancesMonitoringEnabled :: Maybe Bool
    , requestSpotInstancesSubnetId :: Maybe Text
    , requestSpotInstancesNetworkInterfaces
        :: [NetworkInterfaceParam]
    , requestSpotInstancesIamInstancesProfile
        :: Maybe IamInstanceProfile
    , requestSpotInstancesEbsOptimized :: Maybe Bool
    }
  deriving (Show, Read, Eq)

data CancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse
    { cancelSpotInstanceRequestId :: Text
    , cancelSpotInstanceState :: SpotInstanceState
    }
  deriving (Show, Read, Eq)

deriveFromText "SpotInstanceType" ["one-time", "persistent"]

deriveFromText "SpotInstanceState" ["open", "active", "closed", "cancelled", "failed"]
