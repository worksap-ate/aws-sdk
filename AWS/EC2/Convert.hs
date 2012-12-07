{-# LANGUAGE RankNTypes, TemplateHaskell #-}

module AWS.EC2.Convert
    where

import Data.Text (Text)
import Data.Maybe (fromMaybe)

import AWS.Util
import AWS.Lib.Convert
import AWS.EC2.Types

mkConvertFunc
    "imageState"
    ''ImageState
    ["available", "pending", "failed"]

mkConvertFunc
    "productCodeType'"
    ''ProductCodeType
    ["devpay", "marketplace"]

mkConvertFunc
    "imageType"
    ''ImageType
    ["machine", "kernel", "ramdisk"]

platform :: Maybe Text -> Platform
platform Nothing   = PlatformOther
platform (Just t)
    | t == "windows" = PlatformWindows
    | otherwise      = PlatformOther

mkConvertFunc
    "rootDeviceType"
    ''RootDeviceType
    ["ebs", "instance-store"]

volumeType :: Text -> Maybe Int -> VolumeType
volumeType t Nothing  | t == "standard" = VolumeTypeStandard
volumeType t (Just i) | t == "io1"      = VolumeTypeIO1 i
volumeType t _ = err "volume type" t

mkConvertFunc
    "virtualizationType"
    ''VirtualizationType ["paravirtual", "hvm"]

mkConvertFunc "hypervisor" ''Hypervisor ["ovm", "xen"]

mkConvertFunc
    "instanceStatusEventCode'"
    ''InstanceStatusEventCode
    [ "instance-reboot"
    , "instance-stop"
    , "system-reboot"
    , "instance-retirement"
    ]

mkConvertFunc
    "instanceStatusTypeStatus'"
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

mkConvertFunc
    "instanceMonitoringState"
    ''InstanceMonitoringState
    ["disabled", "enabled", "pending"]

mkConvertFunc "architecture" ''Architecture ["i386", "x86_64"]

addressDomain' :: Maybe Text -> AddressDomain
addressDomain' Nothing = AddressDomainStandard
addressDomain' (Just t)
    | t == "standard" = AddressDomainStandard
    | t == "vpc"      = AddressDomainVPC
    | otherwise       = err "address domain" t

ec2Return :: Text -> EC2Return
ec2Return t
    | t == "true" = EC2Success
    | otherwise   = EC2Error t

mkConvertFunc
    "snapshotStatus'"
    ''SnapshotStatus
    ["pending", "completed", "error"]

mkConvertFunc
    "volumeStatus'"
    ''VolumeState
    [ "creating"
    , "available"
    , "in-use"
    , "deleting"
    , "deleted"
    , "error"]

mkConvertFunc
    "attachmentSetItemResponseStatus'"
    ''AttachmentSetItemResponseStatus
    ["attaching", "attached", "detaching", "detached"]

mkConvertFunc
    "shutdownBehavior"
    ''ShutdownBehavior
    ["stop", "terminate"]

mkConvertFunc
    "vpnConnectionState'"
    ''VpnConnectionState
    ["pending", "available", "deleting", "deleted"]

mkConvertFunc
    "vpnTunnelTelemetryStatus'"
    ''VpnTunnelTelemetryStatus
    ["UP", "DOWN"]

mkConvertFunc
    "vpnStaticRouteSource'"
    ''VpnStaticRouteSource
    ["Static"]

mkConvertFunc
    "vpnStaticRouteState'"
    ''VpnStaticRouteState
    ["pending", "available", "deleting", "deleted"]

instanceLifecycle :: Maybe Text -> InstanceLifecycle
instanceLifecycle Nothing = LifecycleNone
instanceLifecycle (Just t)
    | t == "spot"   = LifecycleSpot
    | otherwise     = err "lifecycle" t

mkConvertFunc
    "subnetState'"
    ''SubnetState
    ["pending", "available"]

mkConvertFunc
    "volumeStatusInfoStatus'"
    ''VolumeStatusInfoStatus
    ["ok", "impaired", "insufficient-data"]

mkConvertFunc
    "networkAclRuleAction"
    ''NetworkAclRuleAction
    ["allow", "deny"]

mkConvertFunc
    "routeState'"
    ''RouteState
    ["active", "blackhole"]

mkConvertFunc
    "routeOrigin'"
    ''RouteOrigin
    [ "CreateRouteTable"
    , "CreateRoute"
    , "EnableVgwRoutePropagation"
    ]

mkConvertFunc
    "vpcState'"
    ''VpcState
    ["pending", "available"]

mkConvertFunc
    "vpnGatewayState'"
    ''VpnGatewayState
    ["pending", "available", "deleting", "deleted"]

mkConvertFunc
    "attachmentState'"
    ''AttachmentState
    ["attaching", "attached", "detaching", "detached"]

mkConvertFunc
    "customerGatewayState'"
    ''CustomerGatewayState
    ["pending", "available", "deleting", "deleted"]

mkConvertFunc
    "internetGatewayAttachmentState'"
    ''InternetGatewayAttachmentState
    ["attaching", "attached", "detaching", "detached", "available"]

mkConvertFunc
    "networkInterfaceStatus'"
    ''NetworkInterfaceStatus
    ["available", "in-use"]
