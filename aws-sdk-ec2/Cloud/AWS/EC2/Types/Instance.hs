{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.EC2.Types.Instance
    ( ConsoleOutput(..)
    , EbsInstanceBlockDevice(..)
    , IamInstanceProfile(..)
    , Instance(..)
    , InstanceAttribute(..)
    , InstanceAttributeRequest(..)
    , InstanceBlockDeviceMapping(..)
    , InstanceLifecycle(..)
    , InstanceMonitoringState(..)
    , InstanceNetworkInterface(..)
    , InstanceNetworkInterfaceAssociation(..)
    , InstanceNetworkInterfaceAttachment(..)
    , InstancePrivateIpAddress(..)
    , InstanceState(..)
    , InstanceStateChange(..)
    , InstanceStatus(..)
    , InstanceStatusDetail(..)
    , InstanceStatusDetailName
    , InstanceStatusDetailStatus
    , InstanceStatusEvent(..)
    , InstanceStatusEventCode(..)
    , InstanceStatusType(..)
    , InstanceStatusTypeStatus(..)
    , ModifyInstanceAttributeRequest(..)
    , MonitorInstancesResponse(..)
    , PasswordData(..)
    , Reservation(..)
    , ResetInstanceAttributeRequest(..)
    , RunInstancesRequest(..)
    ) where

import Data.ByteString (ByteString)

import Cloud.AWS.EC2.Types.Common
import Cloud.AWS.EC2.Types.Image (BlockDeviceMappingParam)
import Cloud.AWS.EC2.Types.NetworkInterface (NetworkInterfaceParam)
import Cloud.AWS.EC2.Types.Volume (AttachmentSetItemResponseStatus)

import Cloud.AWS.Lib.FromText (FromText(..), failText, deriveFromText)
import Data.IP (IPv4)
import Data.Text (Text)
import Data.Time (UTCTime)

data ConsoleOutput = ConsoleOutput
    { consoleOutputInstanceId :: Text
    , consoleOutputTimestamp :: UTCTime
        -- ^ The time the data was last updated.
    , consoleOutputOutput :: Text
    }
  deriving (Show, Read, Eq)

data EbsInstanceBlockDevice = EbsInstanceBlockDevice
    { ebsInstanceBlockDeviceVolumeId :: Maybe Text
    , ebsInstanceBlockDeviceState :: Maybe AttachmentSetItemResponseStatus
    , ebsInstanceBlockDeviceAttachTime :: Maybe UTCTime
    , ebsInstanceBlockDeviceDeleteOnTermination :: Maybe Bool
    }
  deriving (Show, Read, Eq)

data IamInstanceProfile = IamInstanceProfile
    { iamInstanceProfileArn :: Text
    , iamInstanceProfileId :: Text
    }
  deriving (Show, Read, Eq)

data Instance = Instance
    { instanceId :: Text
    , instanceImageId :: Text
    , instanceState :: InstanceState
    , instancePrivateDnsName :: Maybe Text
    , instanceDnsName :: Maybe Text
    , instanceReason :: Maybe Text
    , instanceKeyName :: Maybe Text
    , instanceAmiLaunchIndex :: Text
    , instanceProductCodes :: [ProductCode]
    , instanceType :: Text
    , instanceLaunchTime :: UTCTime
    , instancePlacement :: Placement
    , instanceKernelId :: Maybe Text
    , instanceRamdiskId :: Maybe Text
    , instancePlatform :: Maybe Text
    , instanceMonitoring :: InstanceMonitoringState
    , instanceSubnetId :: Maybe Text
    , instanceVpcId :: Maybe Text
    , instancePrivateIpAddress :: Maybe IPv4
    , instanceIpAddress :: Maybe IPv4
    , instanceSourceDestCheck :: Maybe Bool
    , instancevpcGroupSet :: [Group]
    , instanceStateReason :: Maybe StateReason
    , instanceArchitecture :: Architecture
    , instanceRootDeviceType :: RootDeviceType
    , instanceRootDeviceName :: Maybe Text
    , instanceBlockDeviceMappings :: [InstanceBlockDeviceMapping]
    , instanceInstanceLifecycle :: Maybe InstanceLifecycle
    , instanceSpotInstanceRequestId :: Maybe Text
    , instanceVirtualizationType :: VirtualizationType
    , instanceClientToken :: Maybe Text
    , instanceTagSet :: [ResourceTag]
    , instanceHypervisor :: Hypervisor
    , instanceNetworkInterfaceSet :: [InstanceNetworkInterface]
    , instanceIamInstanceProfile :: Maybe IamInstanceProfile
    , instanceEbsOptimized :: Bool -- default: false
    }
  deriving (Show, Read, Eq)

data InstanceAttribute
    = InstanceAttributeInstanceType Text
    | InstanceAttributeKernelId (Maybe Text)
    | InstanceAttributeRamdiskId (Maybe Text)
    | InstanceAttributeUserData (Maybe Text)
    | InstanceAttributeDisableApiTermination Bool
    | InstanceAttributeShutdownBehavior ShutdownBehavior
    | InstanceAttributeRootDeviceName (Maybe Text)
    | InstanceAttributeBlockDeviceMapping [InstanceBlockDeviceMapping]
    | InstanceAttributeSourceDestCheck (Maybe Bool)
    | InstanceAttributeGroupSet [Text]
    | InstanceAttributeProductCodes [ProductCode]
    | InstanceAttributeEbsOptimized Bool
  deriving (Show, Read, Eq)

data InstanceAttributeRequest
    = InstanceAttributeRequestInstanceType
    | InstanceAttributeRequestKernelId
    | InstanceAttributeRequestRamdiskId
    | InstanceAttributeRequestUserData
    | InstanceAttributeRequestDisableApiTermination
    | InstanceAttributeRequestShutdownBehavior
    | InstanceAttributeRequestRootDeviceName
    | InstanceAttributeRequestBlockDeviceMapping
    | InstanceAttributeRequestSourceDestCheck
    | InstanceAttributeRequestGroupSet
    | InstanceAttributeRequestProductCodes
    | InstanceAttributeRequestEbsOptimized
  deriving (Show, Eq, Ord)

data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping
    { instanceBlockDeviceMappingDeviceName :: Text
    , instanceBlockDeviceMappingEbs :: EbsInstanceBlockDevice
    }
  deriving (Show, Read, Eq)

data InstanceLifecycle
    = LifecycleSpot
    | LifecycleNone
  deriving (Show, Read, Eq)

instance FromText InstanceLifecycle where
    fromText t
        | t == "spot" = return LifecycleSpot
        | otherwise   = fail "no Instance lifecycle"
    fromNamedText _name Nothing  = return LifecycleNone
    fromNamedText _name (Just t)
        | t == "spot" = return LifecycleSpot
        | otherwise   = failText t

data InstanceMonitoringState
    = MonitoringDisabled
    | MonitoringEnabled
    | MonitoringPending
    | MonitoringDisabling
  deriving (Show, Read, Eq)

data InstanceNetworkInterface = InstanceNetworkInterface
    { instanceNetworkInterfaceId :: Maybe Text
    , instanceNetworkInterfaceSubnetId :: Maybe Text
    , instanceNetworkInterfaceVpcId :: Maybe Text
    , instanceNetworkInterfaceDescription :: Maybe Text
    , instanceNetworkInterfaceOwnerId :: Maybe Text
    , instanceNetworkInterfaceStatus :: Maybe Text
    , instanceNetworkInterfaceMacAddress :: Maybe Text
    , instanceNetworkInterfacePrivateIpAddress :: Maybe IPv4
    , instanceNetworkInterfacePrivateDnsName :: Maybe Text
    , instanceNetworkInterfaceSourceDestCheck :: Maybe Bool
    , instanceNetworkInterfaceGroupSet :: [Group]
    , instanceNetworkInterfaceAttachment
        :: Maybe InstanceNetworkInterfaceAttachment
    , instanceNetworkInterfaceAssociation
        :: Maybe InstanceNetworkInterfaceAssociation
    , instanceNetworkInterfacePrivateIpAddressesSet
        :: [InstancePrivateIpAddress]
    }
  deriving (Show, Read, Eq)

data InstanceNetworkInterfaceAssociation
    = InstanceNetworkInterfaceAssociation
    { instanceNetworkInterfaceAssociationPublicIp :: IPv4
    , instanceNetworkInterfaceAssociationPublicDnsName
        :: Maybe Text
    , instanceNetworkInterfaceAssociationIpOwnerId :: Text
    }
  deriving (Show, Read, Eq)

data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment
    { instanceNetworkInterfaceAttachmentId :: Text
    , instanceNetworkInterfaceAttachmentDeviceIndex :: Int
    , instanceNetworkInterfaceAttachmentStatus :: Text
    , instanceNetworkInterfaceAttachmentAttachTime :: UTCTime
    , instanceNetworkInterfaceAttachmentDeleteOnTermination
        :: Bool
    }
  deriving (Show, Read, Eq)

data InstancePrivateIpAddress = InstancePrivateIpAddress
    { instancePrivateIpAddressAddress :: IPv4
    , instancePrivateIpAddressDnsName :: Maybe Text
    , instancePrivateIpAddressPrimary :: Bool
    , instancePrivateIpAddressAssociation
        :: Maybe InstanceNetworkInterfaceAssociation
    }
  deriving (Show, Read, Eq)

data InstanceState
    = InstanceStatePending
    | InstanceStateRunning
    | InstanceStateShuttingDown
    | InstanceStateTerminated
    | InstanceStateStopping
    | InstanceStateStopped
    | InstanceStateUnknown Int
  deriving (Show, Read, Eq)

data InstanceStateChange = InstanceStateChange
    { instanceStateChangeInstanceId :: Text
    , instanceStateChangeCurrentState :: InstanceState
    , instanceStateChangePreviousState :: InstanceState
    }
  deriving (Show, Read, Eq)

data InstanceStatus = InstanceStatus
    { instanceStatusInstanceId :: Text
    , instanceStatusAvailabilityZone :: Text
    , instanceStatusEventsSet :: [InstanceStatusEvent]
    , instanceStatusInstanceState :: InstanceState
    , instanceStatusSystemStatus :: InstanceStatusType
    , instanceStatusInstanceStatus :: InstanceStatusType
    }
  deriving (Show, Read, Eq)

data InstanceStatusDetail = InstanceStatusDetail
    { instanceStatusDetailName :: InstanceStatusDetailName
    , instanceStatusDetailStatus :: InstanceStatusDetailStatus
    , instanceStatusDetailImpairedSince :: Maybe UTCTime
    }
  deriving (Show, Read, Eq)

type InstanceStatusDetailName = Text

type InstanceStatusDetailStatus = Text

data InstanceStatusEvent = InstanceStatusEvent
    { instanceStatusEventCode :: InstanceStatusEventCode
    , instanceStatusEventDescription :: Text
    , instanceStatusEventNotBefore :: Maybe UTCTime
    , instanceStatusEventNotAfter :: Maybe UTCTime
    }
  deriving (Show, Read, Eq)

data InstanceStatusEventCode
    = InstanceStatusEventCodeInstanceReboot
    | InstanceStatusEventCodeInstanceStop
    | InstanceStatusEventCodeSystemReboot
    | InstanceStatusEventCodeInstanceRetirement
  deriving (Show, Read, Eq)

data InstanceStatusType = InstanceStatusType
    { instanceStatusTypeStatus :: InstanceStatusTypeStatus
    , instanceStatusTypeDetails :: [InstanceStatusDetail]
    }
  deriving (Show, Read, Eq)

data InstanceStatusTypeStatus
    = InstanceStatusTypeStatusOK
    | InstanceStatusTypeStatusImpaired
    | InstanceStatusTypeStatusInsufficientData
    | InstanceStatusTypeStatusNotApplicable
    | InstanceStatusTypeStatusInitializing
  deriving (Show, Read, Eq)

data ModifyInstanceAttributeRequest
    = ModifyInstanceAttributeRequestInstanceType Text
    | ModifyInstanceAttributeRequestKernelId Text
    | ModifyInstanceAttributeRequestRamdiskId Text
    | ModifyInstanceAttributeRequestUserData Text
    | ModifyInstanceAttributeRequestDisableApiTermination Bool
    | ModifyInstanceAttributeRequestShutdownBehavior
        ShutdownBehavior
    | ModifyInstanceAttributeRequestRootDeviceName Text
    | ModifyInstanceAttributeRequestBlockDeviceMapping
        [BlockDeviceMappingParam]
    | ModifyInstanceAttributeRequestSourceDestCheck Bool
    | ModifyInstanceAttributeRequestGroupSet [Text]
    | ModifyInstanceAttributeRequestEbsOptimized Bool
  deriving (Show, Read, Eq)


data MonitorInstancesResponse = MonitorInstancesResponse
    { monitorInstancesResponseInstanceId :: Text
    , monitorInstancesResponseInstanceMonitoringState
        :: InstanceMonitoringState
    }
  deriving (Show, Read, Eq)

data PasswordData = PasswordData
    { passwordDataInstanceId :: Text
    , passwordDataTimestamp :: UTCTime
      -- ^ The time the data was last updated.
    , passwordDataPasswordData :: Text
    }
  deriving (Show, Read, Eq)

data Reservation = Reservation
    { reservationId :: Text
    , reservationOwnerId :: Text
    , reservationGroupSet :: [Group]
    , reservationInstanceSet :: [Instance]
    , reservationRequesterId :: Maybe Text
    }
  deriving (Show, Read, Eq)

data ResetInstanceAttributeRequest
    = ResetInstanceAttributeRequestKernel
    | ResetInstanceAttributeRequestRamdisk
    | ResetInstanceAttributeRequestSourceDestCheck
  deriving (Show, Read, Eq)

data RunInstancesRequest = RunInstancesRequest
    { runInstancesRequestImageId :: Text -- ^ Required
    , runInstancesRequestMinCount :: Int -- ^ Required
    , runInstancesRequestMaxCount :: Int -- ^ Required
    , runInstancesRequestKeyName :: Maybe Text
    , runInstancesRequestSecurityGroupIds :: [Text]
      -- ^ SecurityGroupIds (Required for VPC; optional for EC2)
    , runInstancesRequestSecurityGroups :: [Text]
      -- ^ SecurityGroups (Only for EC2; either id or name is accepted)
    , runInstancesRequestUserData :: Maybe ByteString
      -- ^ UserData (Base64-encoded MIME user data)
    , runInstancesRequestInstanceType :: Maybe Text
    , runInstancesRequestAvailabilityZone :: Maybe Text
    , runInstancesRequestPlacementGroup :: Maybe Text
    , runInstancesRequestTenancy :: Maybe Text
    , runInstancesRequestKernelId :: Maybe Text
    , runInstancesRequestRamdiskId :: Maybe Text
    , runInstancesRequestBlockDeviceMappings
        :: [BlockDeviceMappingParam]
    , runInstancesRequestMonitoringEnabled :: Maybe Bool
    , runInstancesRequestSubnetId :: Maybe Text
    , runInstancesRequestDisableApiTermination :: Maybe Bool
    , runInstancesRequestShutdownBehavior
        :: Maybe ShutdownBehavior
    , runInstancesRequestPrivateIpAddress :: Maybe IPv4
    , runInstancesRequestClientToken :: Maybe Text
    , runInstancesRequestNetworkInterfaces
        :: [NetworkInterfaceParam]
    , runInstancesRequestIamInstanceProfile
        :: Maybe IamInstanceProfile
    , runInstancesRequestEbsOptimized :: Maybe Bool
    }
  deriving (Show, Read, Eq)

deriveFromText "InstanceMonitoringState"
    ["disabled", "enabled", "pending", "disabling"]
deriveFromText "InstanceStatusEventCode"
    [ "instance-reboot"
    , "instance-stop"
    , "system-reboot"
    , "instance-retirement"
    ]
deriveFromText "InstanceStatusTypeStatus"
    ["ok", "impaired", "insufficient-data", "not-applicable", "initializing"]
