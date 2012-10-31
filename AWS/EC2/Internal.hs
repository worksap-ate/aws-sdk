{-# LANGUAGE RankNTypes #-}

module AWS.EC2.Internal
    where

import Control.Monad.IO.Class (MonadIO)
import qualified Network.HTTP.Conduit as HTTP
import Data.ByteString.Char8 ()
import Control.Applicative
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.XML.Types (Event)
import Data.Text (Text)

import AWS.Class
import AWS.Credential
import AWS.Util
import AWS.Lib.Parser
import AWS.EC2.Types

initialEC2Context :: HTTP.Manager -> AWSContext
initialEC2Context mgr = AWSContext
    { manager = mgr
    , endpoint = "ec2.amazonaws.com"
    , lastRequestId = Nothing
    }

type EC2 m a = AWS AWSContext m a

runEC2 :: MonadIO m => Credential -> AWS AWSContext m a -> m a
runEC2 = runAWS initialEC2Context

itemConduit :: MonadThrow m
    => Text
    -> GLSink Event m o
    -> GLConduit Event m o
itemConduit tag inner =
    maybe (()) id <$> elementM tag (listConduit "item" inner)

itemsSet :: MonadThrow m
    => Text
    -> GLSink Event m o
    -> GLSink Event m [o]
itemsSet tag inner = itemConduit tag inner >+> CL.consume

resourceTagSink :: MonadThrow m
    => GLSink Event m [ResourceTag]
resourceTagSink = itemsSet "tagSet" $
    ResourceTag
    <$> getT "key"
    <*> getMT "value"

productCodeSink :: MonadThrow m
    => GLSink Event m [ProductCode]
productCodeSink = itemsSet "productCodes" $
    ProductCode
    <$> getT "productCode"
    <*> getF "type" productCodeType

stateReasonSink :: MonadThrow m
    => GLSink Event m (Maybe StateReason)
stateReasonSink = elementM "stateReason" $
    StateReason
    <$> getT "code"
    <*> getT "message"

volumeTypeSink :: MonadThrow m
    => GLSink Event m VolumeType
volumeTypeSink = volumeType
    <$> getT "volumeType"
    <*> getM "iops" (textToInt <$>)

returnBool :: MonadThrow m => GLSink Event m Bool
returnBool = getF "return" textToBool

imageState :: Text -> ImageState
imageState a
    | a == "available" = ImageAvailable
    | a == "pending"   = ImagePending
    | a == "failed"    = ImageFailed
    | otherwise        = err "image state" a

productCodeType :: Text -> ProductCodeType
productCodeType t
    | t == "marketplace" = Marketplace
    | t == "devpay"      = Devpay
    | otherwise          = err "product code type" t

imageType :: Text -> ImageType
imageType t
    | t == "machine"  = Machine
    | t == "kernel"   = Kernel
    | t == "ramdisk" = RamDisk
    | otherwise       = err "image type" t

platform :: Maybe Text -> Platform
platform Nothing   = Other
platform (Just t)
    | t == "windows" = Windows
    | otherwise      = Other

rootDeviceType :: Text -> RootDeviceType
rootDeviceType t
    | t == "ebs"            = EBS
    | t == "instance-store" = InstanceStore
    | otherwise             = err "root device type" t

volumeType :: Text -> Maybe Int -> VolumeType
volumeType t Nothing  | t == "standard" = Standard
volumeType t (Just i) | t == "io1"      = IO1 i
volumeType t _ = err "volume type" t

virtualizationType :: Text -> VirtualizationType
virtualizationType t
    | t == "paravirtual" = Paravirtual
    | t == "hvm"         = HVM
    | otherwise          = err "virtualization type" t

hypervisor :: Text -> Hypervisor
hypervisor t
    | t == "xen" = Xen
    | t == "ovm" = OVM
    | otherwise  = err "hypervisor" t

instanceStatusEventCode :: Text -> InstanceStatusEventCode
instanceStatusEventCode t
    | t == "instance-reboot"     = InstanceReboot
    | t == "instance-stop"       = InstanceStop
    | t == "system-reboot"       = SystemReboot
    | t == "instance-retirement" = InstanceRetirement
    | otherwise                  = err "InstanceStatusEventCode" t

instanceStatusTypeStatus :: Text -> InstanceStatusTypeStatus
instanceStatusTypeStatus t
    | t == "ok"                = InstanceStatusOK
    | t == "impaired"          = InstanceStatusImpaired
    | t == "insufficient-data" = InstanceStatusInsufficientData
    | t == "not-applicable"    = InstanceStatusNotApplicable
    | otherwise = err "instance status detail status" t

instanceStateCodes :: [(Int, InstanceState)]
instanceStateCodes =
    [ (0, Pending)
    , (16, Running)
    , (32, ShuttingDown)
    , (48, Terminated)
    , (64, Stopping)
    , (80, Stopped)
    ]

codeToState :: Int -> InstanceState
codeToState code = case lookup code instanceStateCodes of
    Nothing -> UnknownState code
    Just st -> st

instanceMonitoringState :: Text -> InstanceMonitoringState
instanceMonitoringState t
    | t == "disabled" = MonitoringDisabled
    | t == "enabled"  = MonitoringEnabled
    | t == "pending"  = MonitoringPending
    | otherwise       = err "monitoring state" t

architecture :: Text -> Architecture
architecture t
    | t == "i386"   = I386
    | t == "x86_64" = X86_64
    | otherwise     = err "architecture" t

addressDomain :: Maybe Text -> AddressDomain
addressDomain Nothing = AddressDomainStandard
addressDomain (Just t)
    | t == "standard" = AddressDomainStandard
    | t == "vpc"      = AddressDomainVPC
    | otherwise       = err "address domain" t

ec2Return :: Text -> EC2Return
ec2Return t
    | t == "true" = EC2Success
    | otherwise   = EC2Error t

snapshotStatus :: Text -> SnapshotStatus
snapshotStatus t
    | t == "pending"   = SSPending
    | t == "completed" = SSCompleted
    | t == "error"     = SSError
    | otherwise        = err "snapshot status" t

volumeStatus :: Text -> VolumeState
volumeStatus t
    | t == "creating"  = VolCreating
    | t == "available" = VolAvailable
    | t == "in-use"    = VolInUse
    | t == "deleting"  = VolDeleting
    | t == "deleted"   = VolDeleted
    | t == "error"     = VolError
    | otherwise        = err "volume state" t

attachmentSetItemResponseStatus :: Text -> AttachmentSetItemResponseStatus
attachmentSetItemResponseStatus t
    | t == "attaching" = AsirAttaching
    | t == "attached"  = AsirAttached
    | t == "detaching" = AsirDetaching
    | t == "detached"  = AsirDetached
    | otherwise        = err "attachment status" t

shutdownBehavior :: Text -> ShutdownBehavior
shutdownBehavior t
    | t == "stop"      = SBStop
    | t == "terminate" = SBTerminate
    | otherwise = err "shutdown behavior" t

vpnConnectionState :: Text -> VpnConnectionState
vpnConnectionState t
    | t == "pending"   = VCSPending
    | t == "available" = VCSAvailable
    | t == "deleting"  = VCSDeleting
    | t == "deleted"   = VCSDeleted
    | otherwise        = err "vpn connection state" t

vpnTunnelTelemetryStatus :: Text -> VpnTunnelTelemetryStatus
vpnTunnelTelemetryStatus t
    | t == "UP"   = VTTSUp
    | t == "DOWN" = VTTSDown
    | otherwise   = err "vpn tunnel telemetry status" t

vpnStaticRouteSource :: Text -> VpnStaticRouteSource
vpnStaticRouteSource t
    | t == "Static" = VSRStatic
    | otherwise     = err "vpn static route source" t

vpnStaticRouteState :: Text -> VpnStaticRouteState
vpnStaticRouteState t
    | t == "pending"   = VSRSPending
    | t == "available" = VSRSAvailable
    | t == "deleting"  = VSRSDeleting
    | t == "deleted"   = VSRSDeleted
    | otherwise        = err "vpn static route state" t

instanceLifecycle :: Maybe Text -> InstanceLifecycle
instanceLifecycle Nothing = LifecycleNone
instanceLifecycle (Just t)
    | t == "spot"   = LifecycleSpot
    | otherwise     = err "lifecycle" t

subnetState :: Text -> SubnetState
subnetState t
    | t == "pending"   = SubnetPending
    | t == "available" = SubnetAvailable
    | otherwise        = err "subnet state" t

volumeStatusInfoStatus :: Text -> VolumeStatusInfoStatus
volumeStatusInfoStatus t
    | t == "ok"                = VSIOK
    | t == "impaired"          = VSIImpaired
    | t == "insufficient-data" = VSIInsufficientData
    | otherwise                = err "VolumeStatusInfo Status" t

networkAclRuleAction :: Text -> NetworkAclRuleAction
networkAclRuleAction t
    | t == "allow" = NetworkAclRuleActionAllow
    | t == "deny"  = NetworkAclRuleActionDeny
    | otherwise    = err "network acl rule action" t

routeState :: Text -> RouteState
routeState t
    | t == "active"    = RouteStateActive
    | t == "blackhole" = RouteStateBlackhole
    | otherwise        = err "Route State" t

routeOrigin :: Text -> RouteOrigin
routeOrigin t
    | t == "CreateRouteTable"          = RouteOriginCreateRouteTable
    | t == "CreateRoute"               = RouteOriginCreateRoute
    | t == "EnableVgwRoutePropagation" = RouteOriginTableEnableVgwRoutePropagation
    | otherwise                        = err "Route Origin" t

vpcState' :: Text -> VpcState
vpcState' t
    | t == "pending"   = VpcStatePending
    | t == "available" = VpcStateAvailable
    | otherwise        = err "Vpc State" t

vpnGatewayState' :: Text -> VpnGatewayState
vpnGatewayState' t
    | t == "pending"   = VpnGatewayStatePending
    | t == "available" = VpnGatewayStateAvailable
    | t == "deleting"  = VpnGatewayStateDeleting
    | t == "deleted"   = VpnGatewayStateDeleted
    | otherwise        = err "VpnGateway State" t

attachmentState' :: Text -> AttachmentState
attachmentState' t
    | t == "attaching" = AttachmentStateAttaching
    | t == "attached"  = AttachmentStateAttached
    | t == "detaching" = AttachmentStateDetaching
    | t == "detached"  = AttachmentStateDetached
    | otherwise        = err "Attachment State" t
