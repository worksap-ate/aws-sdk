{-# LANGUAGE RankNTypes, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AWS.EC2.Convert
    where

import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.Conduit (monadThrow)

import AWS.Lib.Convert
import AWS.EC2.Types
import AWS.Class (FromText(..), AWSException(..))

deriveFromText
    ''ImageState
    ["available", "pending", "failed"]

deriveFromText
    ''ProductCodeType
    ["devpay", "marketplace"]

deriveFromText
    ''ImageType
    ["machine", "kernel", "ramdisk"]

instance FromText Platform
  where
    fromMaybeText Nothing  = return PlatformOther
    fromMaybeText (Just t)
        | t == "windows" = return PlatformWindows
        | otherwise      = return PlatformOther

deriveFromText
    ''RootDeviceType
    ["ebs", "instance-store"]

deriveFromText
    ''VirtualizationType ["paravirtual", "hvm"]

deriveFromText ''Hypervisor ["ovm", "xen"]

deriveFromText
    ''InstanceStatusEventCode
    [ "instance-reboot"
    , "instance-stop"
    , "system-reboot"
    , "instance-retirement"
    ]

deriveFromText
    ''InstanceStatusTypeStatus
    ["ok", "impaired", "insufficient-data", "not-applicable"]

instanceStateCodes :: [(Int, InstanceState)]
instanceStateCodes =
    [ (0, InstanceStatePending)
    , (16, InstanceStateRunning)
    , (32, InstanceStateShuttingDown)
    , (48, InstanceStateTerminated)
    , (64, InstanceStateStopping)
    , (80, InstanceStateStopped)
    ]

codeToState :: Int -> Text -> InstanceState
codeToState code _name = fromMaybe
    (InstanceStateUnknown code)
    (lookup code instanceStateCodes)

deriveFromText
    ''InstanceMonitoringState
    ["disabled", "enabled", "pending"]

deriveFromText ''Architecture ["i386", "x86_64"]

instance FromText AddressDomain
  where
    fromMaybeText Nothing  = return AddressDomainStandard
    fromMaybeText (Just t)
        | t == "standard" = return AddressDomainStandard
        | t == "vpc"      = return AddressDomainVPC
        | otherwise       = monadThrow $ TextConversionException t

instance FromText EC2Return
  where
    fromTextMay t
        | t == "true" = Just EC2Success
        | otherwise   = Just $ EC2Error t

deriveFromText
    ''SnapshotStatus
    ["pending", "completed", "error"]

deriveFromText
    ''VolumeState
    [ "creating"
    , "available"
    , "in-use"
    , "deleting"
    , "deleted"
    , "error"]

deriveFromText
    ''AttachmentSetItemResponseStatus
    ["attaching", "attached", "detaching", "detached"]

deriveFromText
    ''ShutdownBehavior
    ["stop", "terminate"]

deriveFromText
    ''VpnConnectionState
    ["pending", "available", "deleting", "deleted"]

deriveFromText
    ''VpnTunnelTelemetryStatus
    ["UP", "DOWN"]

deriveFromText
    ''VpnStaticRouteSource
    ["Static"]

deriveFromText
    ''VpnStaticRouteState
    ["pending", "available", "deleting", "deleted"]

instance FromText InstanceLifecycle
  where
    fromMaybeText Nothing  = return LifecycleNone
    fromMaybeText (Just t)
        | t == "spot" = return LifecycleSpot
        | otherwise   = monadThrow $ TextConversionException t

deriveFromText
    ''SubnetState
    ["pending", "available"]

deriveFromText
    ''VolumeStatusInfoStatus
    ["ok", "impaired", "insufficient-data"]

deriveFromText
    ''NetworkAclRuleAction
    ["allow", "deny"]

deriveFromText
    ''RouteState
    ["active", "blackhole"]

deriveFromText
    ''RouteOrigin
    [ "CreateRouteTable"
    , "CreateRoute"
    , "EnableVgwRoutePropagation"
    ]

deriveFromText
    ''VpcState
    ["pending", "available"]

deriveFromText
    ''VpnGatewayState
    ["pending", "available", "deleting", "deleted"]

deriveFromText
    ''AttachmentState
    ["attaching", "attached", "detaching", "detached"]

deriveFromText
    ''CustomerGatewayState
    ["pending", "available", "deleting", "deleted"]

deriveFromText
    ''InternetGatewayAttachmentState
    ["attaching", "attached", "detaching", "detached", "available"]

deriveFromText
    ''NetworkInterfaceStatus
    ["available", "in-use"]
