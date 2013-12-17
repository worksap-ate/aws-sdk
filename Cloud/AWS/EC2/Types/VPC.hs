{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.EC2.Types.VPC
    ( Attachment(..)
    , AttachmentState(..)
    , CreateVpnGatewayType(..)
    , CustomerGateway(..)
    , CustomerGatewayState(..)
    , DhcpConfiguration(..)
    , DhcpOptions(..)
    , DhcpValue(..)
    , InternetGateway(..)
    , InternetGatewayAttachment(..)
    , InternetGatewayAttachmentState(..)
    , Vpc(..)
    , VpcState(..)
    , VpnConnection(..)
    , VpnConnectionOptionsRequest(..)
    , VpnConnectionState(..)
    , VpnGateway(..)
    , VpnGatewayState(..)
    , VpnStaticRoute(..)
    , VpnStaticRouteSource(..)
    , VpnStaticRouteState(..)
    , VpnTunnelTelemetry(..)
    ) where

import Cloud.AWS.EC2.Types.Common (ResourceTag)
import Cloud.AWS.Lib.FromText (deriveFromText)
import Cloud.AWS.Lib.ToText (deriveToText)
import Data.IP (AddrRange, IPv4)
import Data.Text (Text)
import Data.Time (UTCTime)

data Attachment = Attachment
    { attachmentVpcId :: Text
    , attachmentState :: AttachmentState
    }
  deriving (Show, Read, Eq)

data AttachmentState
    = AttachmentStateAttaching
    | AttachmentStateAttached
    | AttachmentStateDetaching
    | AttachmentStateDetached
  deriving (Show, Read, Eq)

data CreateVpnGatewayType = CreateVpnGatewayTypeIpsec1

deriveToText "CreateVpnGatewayType" ["ipsec.1"]

data CustomerGateway = CustomerGateway
    { customerGatewayId :: Text
    , customerGatewayState :: CustomerGatewayState
    , customerGatewayType :: Text
    , customerGatewayIpAddress :: IPv4
    , customerGatewayBgpAsn :: Int
    , customerGatewayTagSet :: [ResourceTag]
    }
  deriving (Show, Read, Eq)

data CustomerGatewayState
    = CustomerGatewayStatePending
    | CustomerGatewayStateAvailable
    | CustomerGatewayStateDeleting
    | CustomerGatewayStateDeleted
  deriving (Show, Read, Eq)

data DhcpOptions = DhcpOptions
    { dhcpOptionsId :: Text
    , dhcpOptionsDhcpConfigurationSet :: [DhcpConfiguration]
    , dhcpOptionsTagSet :: [ResourceTag]
    }
  deriving (Show, Read, Eq)

data DhcpConfiguration = DhcpConfiguration
    { dhcpConfigurationKey :: Text
    , dhcpConfigurationDhcpValueSet :: [DhcpValue]
    }
  deriving (Show, Read, Eq)

data DhcpValue = DhcpValue
    { dhcpValueValue :: Text
    }
  deriving (Show, Read, Eq)

data InternetGateway = InternetGateway
    { internetGatewayInternetGatewayId :: Text
    , internetGatewayAttachmentSet :: [InternetGatewayAttachment]
    , internetGatewayTagSet :: [ResourceTag]
    }
  deriving (Show, Read, Eq)

data InternetGatewayAttachment = InternetGatewayAttachment
    { internetGatewayAttachmentVpcId :: Text
    , internetGatewayAttachmentState :: InternetGatewayAttachmentState
    }
  deriving (Show, Read, Eq)

data InternetGatewayAttachmentState
    = InternetGatewayAttachmentStateAttaching
    | InternetGatewayAttachmentStateAttached
    | InternetGatewayAttachmentStateDetaching
    | InternetGatewayAttachmentStateDetached
    | InternetGatewayAttachmentStateAvailable
  deriving (Show, Read, Eq)

data Vpc = Vpc
    { vpcId :: Text
    , vpcState :: VpcState
    , vpcCidrBlock :: AddrRange IPv4
    , vpcDhcpOptionsId :: Text
    , vpcTagSet :: [ResourceTag]
    , vpcInstanceTenancy :: Text
    , vpcIsDefault :: Maybe Text
    }
  deriving (Show, Read, Eq)

data VpcState
    = VpcStatePending
    | VpcStateAvailable
  deriving (Show, Read, Eq)

data VpnConnection = VpnConnection
    { vpnConnectionId :: Text
    , vpnConnectionState :: VpnConnectionState
    , vpnConnectionCustomerGatewayConfiguration :: Maybe Text
    , vpnConnectionType :: Maybe Text
    , vpnConnectionCustomerGatewayId :: Text
    , vpnConnectionVpnGatewayId :: Text
    , vpnConnectionTagSet :: [ResourceTag]
    , vpnConnectionVgwTelemetry :: [VpnTunnelTelemetry]
    , vpnConnectionOptions :: Maybe VpnConnectionOptionsRequest
    , vpnConnectionRoutes :: [VpnStaticRoute]
    }
  deriving (Show, Read, Eq)

data VpnConnectionOptionsRequest = VpnConnectionOptionsRequest
    { vpnConnectionOptionsRequestStaticRoutesOnly :: Bool
    }
  deriving (Show, Read, Eq)

data VpnConnectionState
    = VpnConnectionStatePending
    | VpnConnectionStateAvailable
    | VpnConnectionStateDeleting
    | VpnConnectionStateDeleted
  deriving (Show, Read, Eq)

data VpnGateway = VpnGateway
    { vpnGatewayId :: Text
    , vpnGatewayState :: VpnGatewayState
    , vpnGatewayType :: Text
    , vpnGatewayAvailabilityZone :: Maybe Text
    , vpnGatewayAttachments :: [Attachment]
    , vpnGatewayTagSet :: [ResourceTag]
    }
  deriving (Show, Read, Eq)

data VpnGatewayState
    = VpnGatewayStatePending
    | VpnGatewayStateAvailable
    | VpnGatewayStateDeleting
    | VpnGatewayStateDeleted
  deriving (Show, Read, Eq)

data VpnStaticRoute = VpnStaticRoute
    { vpnStaticRouteDestinationCidrBlock :: Text
    , vpnStaticRouteSource :: VpnStaticRouteSource
    , vpnStaticRouteState :: VpnStaticRouteState
    }
  deriving (Show, Read, Eq)

data VpnStaticRouteSource = VpnStaticRouteSourceStatic
  deriving (Show, Read, Eq)

data VpnStaticRouteState
    = VpnStaticRouteStatePending
    | VpnStaticRouteStateAvailable
    | VpnStaticRouteStateDeleting
    | VpnStaticRouteStateDeleted
  deriving (Show, Read, Eq)

data VpnTunnelTelemetry = VpnTunnelTelemetry
    { vpnTunnelTelemetryOutsideIpAddress :: IPv4
    , vpnTunnelTelemetryStatus :: VpnTunnelTelemetryStatus
    , vpnTunnelTelemetryLastStatusChange :: UTCTime
    , vpnTunnelTelemetryStatusMessage :: Maybe Text
    , vpnTunnelTelemetryAcceptRouteCount :: Int
    }
  deriving (Show, Read, Eq)

data VpnTunnelTelemetryStatus
    = VpnTunnelTelemetryStatusUp
    | VpnTunnelTelemetryStatusDown
  deriving (Show, Read, Eq)

deriveFromText "AttachmentState"
    ["attaching", "attached", "detaching", "detached"]
deriveFromText "CustomerGatewayState"
    ["pending", "available", "deleting", "deleted"]
deriveFromText "InternetGatewayAttachmentState"
    ["attaching", "attached", "detaching", "detached", "available"]
deriveFromText "VpcState" ["pending", "available"]
deriveFromText "VpnConnectionState"
    ["pending", "available", "deleting", "deleted"]
deriveFromText "VpnGatewayState"
    ["pending", "available", "deleting", "deleted"]
deriveFromText "VpnStaticRouteSource" ["static"]
deriveFromText "VpnStaticRouteState"
    ["pending", "available", "deleting", "deleted"]
deriveFromText "VpnTunnelTelemetryStatus" ["UP", "DOWN"]
