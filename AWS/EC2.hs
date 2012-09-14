{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module AWS.EC2
    ( module AWS.EC2.Types
    , EC2Endpoint(..)
    , EC2
    , EC2Context
    , Filter
    , newEC2Context
    , runEC2
    , setEndpoint
    , describeImages
    , describeRegions
    , describeAvailabilityZones
    , describeInstances
    , describeAddresses
    ) where

import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy.Char8 ()

import Data.XML.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Control
import qualified Text.XML.Stream.Parse as XML
import Safe
import Control.Monad.State
import Control.Applicative
import Data.Time
import System.Locale

import AWS.Types
import AWS.EC2.Types
import AWS.EC2.Query

runEC2
    :: (MonadResource m, MonadBaseControl IO m)
    => EC2Context
    -> EC2 m a
    -> m a
runEC2 ctx = flip evalStateT ctx

describeImages
    :: (MonadResource m, MonadBaseControl IO m)
    => [ByteString]
    -> [ByteString]
    -> [ByteString]
    -> [Filter]
    -> EC2 m (EC2Response (Source m Image))
describeImages imageIds owners execby filters =
    ec2Query "DescribeImages" params imagesSetConduit
  where
    params =
        [ ArrayParams "ImageId" imageIds
        , ArrayParams "Owner" owners
        , ArrayParams "ExecutableBy" execby
        , FilterParams filters
        ]

imagesSetConduit :: MonadThrow m
    => GLConduit Event m Image
imagesSetConduit = itemConduit "imagesSet" imageItem

itemConduit :: MonadThrow m
    => Text
    -> GLSink Event m o
    -> GLConduit Event m o
itemConduit tag inner =
    element tag (items inner)
  where
    items :: MonadThrow m
        => Pipe Event Event o u m o
        -> Pipe Event Event o u m ()
    items p = awaitWhile isTag >>= maybe (return ()) (\e -> do
        leftover e
        if isBeginTagName "item" e
            then do
                element "item" $ p >>= yield
                items p
            else return ()
        )

itemsSet :: MonadThrow m
    => Text
    -> GLSink Event m o
    -> GLSink Event m [o]
itemsSet tag inner = itemConduit tag inner >+> CL.consume

isTag :: Event -> Bool
isTag (EventBeginElement _ _) =True
isTag (EventEndElement _) =True
isTag _ = False

isBeginTagName :: Text -> Event -> Bool
isBeginTagName name (EventBeginElement n _)
    | n == ec2Name name = True
    | otherwise         = False
isBeginTagName _ _ = False

awaitWhile :: Monad m
    => (i -> Bool)
    -> Pipe l i o u m (Maybe i)
awaitWhile f = 
    await >>= maybe (return Nothing) (
        \a -> if f a
            then return $ Just a
            else awaitWhile f
        )

imageItem :: MonadThrow m
    => GLSink Event m Image
imageItem = image
    <$> getT "imageId"
    <*> getT "imageLocation"
    <*> getF "imageState" t2imageState
    <*> getT "imageOwnerId"
    <*> getF "isPublic" t2bool
    <*> productCodeSink
    <*> getT "architecture"
    <*> getF "imageType" t2imageType
    <*> getMT "kernelId"
    <*> getMT "ramdiskId"
    <*> getM "platform" t2platform
    <*> stateReasonSink
    <*> getMT "imageOwnerAlias"
    <*> getM "name" t2emptxt
    <*> getM "description" t2emptxt
    <*> getF "rootDeviceType" t2rootDeviceType
    <*> getMT "rootDeviceName"
    <*> itemsSet "blockDeviceMapping" (
        blockDeviceMapping
        <$> getT "deviceName"
        <*> getMT "virutalName"
        <*> elementM "ebs" (
            ebsBlockDevice
            <$> getMT "snapshotId"
            <*> getF "volumeSize" t2dec
            <*> getF "deleteOnTermination" t2bool
            <*> getF "volumeType" t2volumeType
            <*> getM "iops" t2iops
            )
        )
    <*> getF "virtualizationType" t2virtualizationType
    <*> resourceTagSink
    <*> getF "hypervisor" t2hypervisor

resourceTagSink :: MonadThrow m
    => GLSink Event m [ResourceTag]
resourceTagSink = itemsSet "tagSet" $
    resourceTag
    <$> getT "key"
    <*> getT "value"

productCodeSink :: MonadThrow m
    => GLSink Event m [ProductCode]
productCodeSink = itemsSet "productCodes" $
    productCode
    <$> getT "productCode"
    <*> getF "type" t2productCodeType

stateReasonSink :: MonadThrow m
    => GLSink Event m (Maybe StateReason)
stateReasonSink = elementM "stateReason" $
    stateReason
    <$> getT "code"
    <*> getT "message"

getF :: MonadThrow m
    => Text
    -> (Text -> b)
    -> Pipe Event Event o u m b
getF name f = tagContentF name >>= return . f

getT :: MonadThrow m
    => Text
    -> Pipe Event Event o u m Text
getT name = getF name id

getM :: MonadThrow m
    => Text
    -> (Maybe Text -> b)
    -> Pipe Event Event o u m b
getM name f = tagContent name >>= return . f

getMT :: MonadThrow m
    => Text
    -> Pipe Event Event o u m (Maybe Text)
getMT name = getM name id

elementM :: MonadThrow m
    => Text
    -> Pipe Event Event o u m a
    -> Pipe Event Event o u m (Maybe a)
elementM name inner = XML.tagNoAttr (ec2Name name) inner

element :: MonadThrow m
    => Text
    -> Pipe Event Event o u m a
    -> Pipe Event Event o u m a
element name inner = XML.force "parse error" $ elementM name inner

err :: String -> Text -> a
err v m = error $ "unknown " ++ v ++ ": " ++ T.unpack m

t2imageState :: Text -> ImageState
t2imageState a
    | a == "available" = ImageAvailable
    | a == "pending"   = ImagePending
    | a == "failed"    = ImageFailed
    | otherwise        = err "image state" a

t2bool :: Text -> Bool
t2bool a
    | a == "true"  = True
    | a == "false" = False
    | otherwise    = err "value" a

t2dec :: Integral a => Text -> a
t2dec t = either 
    (const $ error "not decimal")
    fst
    (decimal t)

t2imageType :: Text -> ImageType
t2imageType t
    | t == "machine"  = Machine
    | t == "kernel"   = Kernel
    | t == "ramdisk" = RamDisk
    | otherwise       = err "image type" t

t2platform :: Maybe Text -> Platform
t2platform Nothing   = Other
t2platform (Just t)
    | t == "windows" = Windows
    | otherwise      = Other

t2rootDeviceType :: Text -> RootDeviceType
t2rootDeviceType t
    | t == "ebs"            = EBS
    | t == "instance-store" = InstanceStore
    | otherwise             = err "root device type" t

t2virtualizationType :: Text -> VirtualizationType
t2virtualizationType t
    | t == "paravirtual" = Paravirtual
    | t == "hvm"         = HVM
    | otherwise          = err "virtualization type" t

t2hypervisor :: Text -> Hypervisor
t2hypervisor t
    | t == "xen" = Xen
    | t == "ovm" = OVM
    | otherwise  = err "hypervisor" t

t2volumeType :: Text -> VolumeType
t2volumeType t
    | t == "standard" = Standard
    | t == "io1"      = IO1
    | otherwise       = err "volume type" t

t2iops :: Maybe Text -> Maybe Int
t2iops mt = mt >>= readMay . T.unpack

t2productCodeType :: Text -> ProductCodeType
t2productCodeType t
    | t == "marketplace" = Marketplace
    | t == "devpay"      = Devpay
    | otherwise          = err "product code type" t

t2time :: Text -> UTCTime
t2time = readTime defaultTimeLocale fmt . T.unpack
  where
    fmt = "%FT%T.000Z"

t2emptxt :: Maybe Text -> Text
t2emptxt = maybe "" id

t2volumeState :: Text -> VolumeState
t2volumeState t
    | t == "attached"  = VolumeAttached
    | t == "attaching" = VolumeAttaching
    | t == "detaching" = VolumeDetaching
    | t == "detached"  = VolumeDetached
    | otherwise        = err "volume state" t

t2architecture :: Text -> Architecture
t2architecture t
    | t == "i386"   = I386
    | t == "x86_64" = X86_64
    | otherwise     = err "architecture" t

t2deviceType :: Text -> RootDeviceType
t2deviceType t
    | t == "ebs"            = EBS
    | t == "instance-store" = InstanceStore
    | otherwise             = err "root device type" t

t2monitoring :: Text -> InstanceMonitoringState
t2monitoring t
    | t == "disabled" = MonitoringDisabled
    | t == "enabled"  = MonitoringEnabled
    | t == "pending"  = MonitoringPending
    | otherwise       = err "monitoring state" t

t2lifecycle :: Maybe Text -> InstanceLifecycle
t2lifecycle Nothing = LifecycleNone
t2lifecycle (Just t)
    | t == "spot"   = LifecycleSpot
    | otherwise     = err "lifecycle" t

{----------------------------------------------------
 - DescribeRegions
 ---------------------------------------------------}
describeRegions
    :: (MonadResource m, MonadBaseControl IO m)
    => [ByteString]
    -> [Filter]
    -> EC2 m (EC2Response (Source m Region))
describeRegions regions filters =
    ec2Query "DescribeRegions" params regionInfoConduit
  where
    params =
        [ ArrayParams "RegionName" regions
        , FilterParams filters
        ]

    regionInfoConduit :: MonadThrow m
        => GLConduit Event m Region
    regionInfoConduit = itemConduit "regionInfo" $
        region
        <$> getT "regionName"
        <*> getT "regionEndpoint"

{----------------------------------------------------
 - DescribeAvailabilityZones
 ---------------------------------------------------}
describeAvailabilityZones
    :: (MonadResource m, MonadBaseControl IO m)
    => [ByteString]
    -> [Filter]
    -> EC2 m (EC2Response (Source m AvailabilityZone))
describeAvailabilityZones zones filters =
    ec2Query "DescribeAvailabilityZones" params availabilityZoneInfo
  where
    params =
        [ ArrayParams "ZoneName" zones
        , FilterParams filters
        ]

    availabilityZoneInfo :: MonadThrow m
        => GLConduit Event m AvailabilityZone
    availabilityZoneInfo = itemConduit "availabilityZoneInfo" $
        availabilityZone
        <$> getT "zoneName"
        <*> getT "zoneState"
        <*> getT "regionName"
        <*> itemsSet "messageSet" (getT "message")

{----------------------------------------------------
 - DescribeInstances
 ---------------------------------------------------}
describeInstances
    :: (MonadResource m, MonadBaseControl IO m)
    => [ByteString]
    -> [Filter]
    -> EC2 m (EC2Response (Source m Reservation))
describeInstances instances filters =
    ec2Query "DescribeInstances" params reservationSet
  where
    params =
        [ ArrayParams "InstanceId" instances
        , FilterParams filters
        ]

reservationSet :: MonadThrow m
    => GLConduit Event m Reservation
reservationSet = itemConduit "reservationSet" $
    reservation
    <$> getT "reservationId"
    <*> getT "ownerId"
    <*> groupSetSink
    <*> instanceSetSink
    <*> getMT "requesterId"

groupSetSink :: MonadThrow m => GLSink Event m [Group]
groupSetSink = itemsSet "groupSet" $ group
    <$> getT "groupId"
    <*> getT "groupName"

instanceSetSink :: MonadThrow m
    => GLSink Event m [Instance]
instanceSetSink = itemsSet "instancesSet" $
    ec2Instance
    <$> getT "instanceId"
    <*> getT "imageId"
    <*> element "instanceState" (
        codeToState
        <$> getF "code" t2dec
        <* getT "name"
        )
    <*> getT "privateDnsName"
    <*> getT "dnsName"
    <*> getT "reason"
    <*> getT "keyName"
    <*> getT "amiLaunchIndex"
    <*> productCodeSink
    <*> getT "instanceType"
    <*> getF "launchTime" t2time
    <*> element "placement" (
        placement
        <$> getT "availabilityZone"
        <*> getT "groupName"
        <*> getT "tenancy"
        )
    <*> getMT "kernelId"
    <*> getMT "ramdiskId"
    <*> getMT "platform"
    <*> element "monitoring" (getF "state" t2monitoring)
    <*> getMT "subnetId"
    <*> getMT "vpcId"
    <*> getMT "privateIpAddress"
    <*> getMT "ipAddress"
    <*> getM "sourceDestCheck" (t2bool <$>)
    <*> groupSetSink
    <*> stateReasonSink
    <*> getF "architecture" t2architecture
    <*> getF "rootDeviceType" t2deviceType
    <*> getT "rootDeviceName"
    <*> itemsSet "blockDeviceMapping" (
        instanceBlockDeviceMapping
        <$> getT "deviceName"
        <*> element "ebs" (
            instanceEbsBlockDevice
            <$> getT "volumeId"
            <*> getF "status" t2volumeState
            <*> getF "attachTime" t2time
            <*> getF "deleteOnTermination" t2bool
            )
        )
    <*> getM "instanceLifecycle" t2lifecycle
    <*> getMT "spotInstanceRequestId"
    <*> getF "virtualizationType" t2virtualizationType
    <*> getT "clientToken"
    <*> resourceTagSink
    <*> getF "hypervisor" t2hypervisor
    <*> networkInterfaceSink
    <*> elementM "iamInstanceProfile" (
        iamInstanceProfile
        <$> getT "arn"
        <*> getT "id"
        )
    <*> getF "ebsOptimized" t2bool

networkInterfaceSink :: MonadThrow m
    => GLSink Event m [InstanceNetworkInterface]
networkInterfaceSink = itemsSet "networkInterfaceSet" $
    instanceNetworkInterface
    <$> getT "networkInterfaceId"
    <*> getT "subnetId"
    <*> getT "vpcId"
    <*> getM "description" t2emptxt
    <*> getT "ownerId"
    <*> getT "status"
    <*> getT "privateIpAddress"
    <*> getMT "privateDnsName"
    <*> getF "sourceDestCheck" t2bool
    <*> groupSetSink
    <*> element "attachment" (
        networkInterfaceAttachment
        <$> getT "attachmentId"
        <*> getF "deviceIndex" t2dec
        <*> getT "status"
        <*> getF "attachTime" t2time
        <*> getF "deleteOnTermination" t2bool
        )
    <*> niAssociationSink
    <*> itemsSet "privateIpAddressesSet" (
        instancePrivateIpAddress
        <$> getT "privateIpAddress"
        <*> getF "primary" t2bool
        <*> niAssociationSink
        )

niAssociationSink :: MonadThrow m
    => GLSink Event m (Maybe NetworkInterfaceAssociation)
niAssociationSink = elementM "association" $
    networkInterfaceAssociation
    <$> getT "publicIp"
    <*> getT "ipOwnerId"

{----------------------------------------------------
 - DescribeAddresses
 ---------------------------------------------------}
describeAddresses
    :: (MonadResource m, MonadBaseControl IO m)
    => [ByteString]
    -> [ByteString]
    -> [Filter]
    -> EC2 m (EC2Response (Source m Address))
describeAddresses pubIps alloIds filters =
    ec2Query "DescribeAddresses" params addressSet
  where
    params =
        [ ArrayParams "PublicIp" pubIps
        , ArrayParams "AllocationId" alloIds
        , FilterParams filters
        ]

    addressSet :: MonadThrow m => GLConduit Event m Address
    addressSet = itemConduit "addressesSet" $ address
        <$> getT "publicIp"
        <*> getMT "allocationId"
        <*> getF "domain" (const AddressDomainStandard)
        <*> getMT "instanceId"
        <*> getMT "associationId"
        <*> getMT "networkInterfaceId"
        <*> getMT "networkInterfaceOwnerId"
        <*> getMT "privateIpAddress"
