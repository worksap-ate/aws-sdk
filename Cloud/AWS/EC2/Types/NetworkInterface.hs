{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.EC2.Types.NetworkInterface
    ( NetworkInterface(..)
    , NetworkInterfaceAssociation(..)
    , NetworkInterfaceAttachment(..)
    , NetworkInterfaceParam(..)
    , NetworkInterfacePrivateIpAddress(..)
    , NetworkInterfaceStatus(..)
    , SecondaryPrivateIpAddressParam(..)
    ) where

import Cloud.AWS.EC2.Types.Common (Group, ResourceTag)
import Cloud.AWS.Lib.FromText (deriveFromText)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.IP (IPv4)

data NetworkInterface = NetworkInterface
    { networkInterfaceId :: Text
    , networkInterfaceSubnetId :: Text
    , networkInterfaceVpcId :: Text
    , networkInterfaceAvailabilityZone :: Text
    , networkInterfaceDescription :: Maybe Text
    , networkInterfaceOwnerId :: Text
    , networkInterfaceRequesterId :: Maybe Text
    , networkInterfaceRequesterManaged :: Text
    , networkInterfaceStatus :: NetworkInterfaceStatus
    , networkInterfaceMacAddress :: Text
    , networkInterfacePrivateIpAddress :: IPv4
    , networkInterfacePrivateDnsName :: Maybe Text
    , networkInterfaceSourceDestCheck :: Bool
    , networkInterfaceGroupSet :: [Group]
    , networkInterfaceAttachment
        :: Maybe NetworkInterfaceAttachment
    , networkInterfaceAssociation
        :: Maybe NetworkInterfaceAssociation
    , networkInterfaceTagSet :: [ResourceTag]
    , networkInterfacePrivateIpAddressesSet
        :: [NetworkInterfacePrivateIpAddress]
    }
  deriving (Show, Read, Eq)

data NetworkInterfaceAssociation = NetworkInterfaceAssociation
    { networkInterfaceAssociationAttachmentId :: Maybe Text
    , networkInterfaceAssociationInstanceId :: Maybe Text
    , networkInterfaceAssociationPublicIp :: IPv4
    , networkInterfaceAssociationPublicDnsName :: Maybe Text
    , networkInterfaceAssociationIpOwnerId :: Text
    , networkInterfaceAssociationId :: Maybe Text
    }
  deriving (Show, Read, Eq)

data NetworkInterfaceAttachment = NetworkInterfaceAttachment
    { networkInterfaceAttachmentId :: Text
    , networkInterfaceAttachmentInstanceId :: Maybe Text
    , networkInterfaceAttachmentInstanceOwnerId :: Text
    , networkInterfaceAttachmentDeviceIndex :: Int
    , networkInterfaceAttachmentStatus :: Text
    , networkInterfaceAttachmentAttachTime :: UTCTime
    , networkInterfaceAttachmentDeleteOnTermination :: Bool
    }
  deriving (Show, Read, Eq)

data NetworkInterfaceParam
    = NetworkInterfaceParamCreate
        { networkInterfaceParamCreateDeviceIndex :: Int
        , networkInterfaceParamCreateSubnetId :: Text
        , networkInterfaceParamCreateDescription :: Text
        , networkInterfaceParamCreatePrivateIpAddress
            :: Maybe IPv4
        , networkInterfaceParamCreatePrivateIpAddresses
            :: SecondaryPrivateIpAddressParam
        , networkInterfaceParamCreateSecurityGroupIds :: [Text]
        , networkInterfaceParamCreateDeleteOnTermination :: Bool
        }
    | NetworkInterfaceParamAttach
        { networkInterfaceParamAttachInterfaceId :: Text
        , networkInterfaceParamAttachDeviceIndex :: Int
        , networkInterfaceParamAttachDeleteOnTermination :: Bool
        }
  deriving (Show, Read, Eq)

data NetworkInterfacePrivateIpAddress
    = NetworkInterfacePrivateIpAddress
    { networkInterfacePrivateIpAddressPrivateIpAddress :: IPv4
    , networkInterfacePrivateIpAddressDnsName :: Maybe Text
    , networkInterfacePrivateIpAddressPrimary :: Bool
    , networkInterfacePrivateIpAddressAssociation
        :: Maybe NetworkInterfaceAssociation
    }
  deriving (Show, Read, Eq)

data NetworkInterfaceStatus
    = NetworkInterfaceStatusAvailable
    | NetworkInterfaceStatusInUse
    | NetworkInterfaceStatusPending
  deriving (Show, Read, Eq)

data SecondaryPrivateIpAddressParam
    = SecondaryPrivateIpAddressParamNothing
    | SecondaryPrivateIpAddressParamCount Int
    | SecondaryPrivateIpAddressParamSpecified
      { secondaryPrivateIpAddressParamSpecifiedAddresses :: [IPv4]
      , secondaryPrivateIpAddressParamSpecifiedPrimary
        :: Maybe Int
      }
  deriving (Show, Read, Eq)

deriveFromText "NetworkInterfaceStatus"
    ["available", "in-use", "pending"]
