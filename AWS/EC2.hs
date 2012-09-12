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
--    element tag (items inner) >>= maybe (return ()) return
    elementF tag (items inner)
  where
    items :: MonadThrow m
        => Pipe Event Event o u m o
        -> Pipe Event Event o u m ()
    items p = awaitWhile isTag >>= maybe (return ()) (\e -> do
        leftover e
        if isBeginTagName "item" e
            then do
                elementF "item" $ p >>= yield
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
imageItem = do
    i <- getT "imageId"
    l <- getT "imageLocation"
    s <- getF "imageState" t2imageState
    oid <- getT "imageOwnerId"
    p <- getF "isPublic" t2bool
    pc <- productCodeSink
    a <- getT "architecture"
    t <- getF "imageType" t2imageType
    kid <- getMT "kernelId"
    rid <- getMT "ramdiskId"
    pf <- getM "platform" t2platform
    sr <- stateReasonSink
    oa <- getMT "imageOwnerAlias"
    n <- getM "name" t2emptxt
    d <- getM "description" t2emptxt
    rdt <- getF "rootDeviceType" t2rootDeviceType
    rdn <- getMT "rootDeviceName"
    bdms <- itemsSet "blockDeviceMapping" $ do
        dname <- getT "deviceName"
        v <- getMT "virutalName"
        e <- element "ebs" $ do
            sid <- getMT "snapshotId"
            vs <- getF "volumeSize" t2dec
            dot <- getF "deleteOnTermination" t2bool
            vt <- getF "volumeType" t2volumeType
            io <- getM "iops" t2iops
            return $ EbsBlockDevice
                { ebsSnapshotId = sid
                , volumeSize = vs
                , ebsDeleteOnTermination = dot
                , volumeType = vt
                , iops = io
                }
        return $ BlockDeviceMapping
            { deviceName = dname
            , virtualName = v
            , ebs = e
            }
    vt <- getF "virtualizationType" t2virtualizationType
    ts <- resourceTagSink
    h <- getF "hypervisor" t2hipervisor
    return $ Image
        { imageId = i
        , imageLocation = l
        , imageState = s
        , imageOwnerId = oid
        , isPublic = p
        , imageProductCodes = pc
        , imageArchitecture = a
        , imageType = t
        , kernelId = kid
        , ramdiskId = rid
        , platform = pf
        , imageStateReason = sr
        , imageOwnerAlias = oa
        , imageName = n
        , description = d
        , rootDeviceType = rdt
        , rootDeviceName = rdn
        , blockDeviceMappings = bdms
        , virtualizationType = vt
        , imageTagSet = ts
        , hipervisor = h
        }

resourceTagSink :: MonadThrow m
    => GLSink Event m [ResourceTag]
resourceTagSink = itemsSet "tagSet" $ do
    k <- getT "key"
    v <- getT "value"
    return ResourceTag
        { resourceKey = k
        , resourceValue = v
        }

productCodeSink :: MonadThrow m
    => GLSink Event m [ProductCode]
productCodeSink = itemsSet "productCodes" $ do
    c <- getT "productCode"
    t <- getF "type" t2productCodeType
    return ProductCode
        { productCode = c
        , productCodeType = t
        }

stateReasonSink :: MonadThrow m
    => GLSink Event m (Maybe StateReason)
stateReasonSink = do
    msr <- element "stateReason" $ do
        c <- getT "code"
        m <- getT "message"
        return StateReason
            { stateReasonCode = c
            , stateReasonMessage = m
            }
    return msr

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

element :: MonadThrow m
    => Text
    -> Pipe Event Event o u m a
    -> Pipe Event Event o u m (Maybe a)
element name inner = XML.tagNoAttr (ec2Name name) inner

elementF :: MonadThrow m
    => Text
    -> Pipe Event Event o u m a
    -> Pipe Event Event o u m a
elementF name inner = XML.force "parse error" $ element name inner

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

t2hipervisor :: Text -> Hipervisor
t2hipervisor t
    | t == "xen" = Xen
    | t == "ovm" = OVM
    | otherwise  = err "hipervisor" t

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
    regionInfoConduit = itemConduit "regionInfo" $ do
        name <- getT "regionName"
        rep <- getT "regionEndpoint"
        return Region
            { regionName = name
            , regionEndpoint = rep
            }

{----------------------------------------------------
 - DescribeRegions
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
    availabilityZoneInfo = itemConduit "availabilityZoneInfo" $ do
        name <- getT "zoneName"
        st <- getT "zoneState"
        region <- getT "regionName"
        msgs <- itemsSet "messageSet" $ getT "message"
        return AvailabilityZone
            { zoneName = name
            , zoneState = st
            , zoneRegionName = region
            , messageSet = msgs
            }

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
reservationSet = itemConduit "reservationSet" $ do
    i <- getT "reservationId"
    o <- getT "ownerId"
    g <- groupSetSink
    iset <- instanceSetSink
    rid <- getMT "requesterId"
    return Reservation
        { reservationId = i
        , ownerId = o
        , groupSet = g
        , instanceSet = iset
        , requesterId = rid
        }

groupSetSink :: MonadThrow m => GLSink Event m [Group]
groupSetSink = itemsSet "groupSet" $ do
    i <- getT "groupId"
    n <- getT "groupName"
    return Group
        { groupId = i
        , groupName = n
        }

instanceSetSink :: MonadThrow m
    => GLSink Event m [Instance]
instanceSetSink = itemsSet "instancesSet" $ do
    iid <- getT "instanceId"
    image <- getT "imageId"
    istate <- elementF "instanceState" $ do
        code <- getF "code" t2dec
        getT "name"
        return $ codeToState code
    pdns <- getT "privateDnsName"
    dns <- getT "dnsName"
    res <- getT "reason"
    kname <- getT "keyName"
    aidx <- getT "amiLaunchIndex"
    pcode <- productCodeSink
    itype <- getT "instanceType"
    ltime <- getF "launchTime" t2time
    placement <- elementF "placement" $ do
        zone <- getT "availabilityZone"
        gname <- getT "groupName"
        ten <- getT "tenancy"
        return Placement
            { availabilityZone = zone
            , placementGroupName = gname
            , tenancy = ten
            }
    kid <- getMT "kernelId"
    rid <- getMT "ramdiskId"
    pf <- getMT "platform"
    mon <- elementF "monitoring" $ getF "state" $ const MonitoringDisabled -- XXX
    snid <- getMT "subnetId"
    vpcid <- getMT "vpcId"
    paddr <- getMT "privateIpAddress"
    addr <- getMT "ipAddress"
    sdc <- getM "sourceDestCheck" (t2bool <$>)
    group <- groupSetSink
    sreason <- stateReasonSink
    arch <- getF "architecture" $ const I386 -- XXX
    rdtype <- getF "rootDeviceType" $ const EBS -- XXX
    rdname <- getT "rootDeviceName"
    bdmap <- itemsSet "blockDeviceMapping" $ do
        devname <- getT "deviceName"
        iebs <- elementF "ebs" $ do
            vid <- getT "volumeId"
            vst <- getF "status" $ const VolumeAttached -- XXX
            atime <- getF "attachTime" $ t2time
            dot <- getF "deleteOnTermination" t2bool
            return InstanceEbsBlockDevice
                { instanceEbsVolumeId = vid
                , instanceEbsState = vst
                , instanceEbsAttachTime = atime
                , instanceEbsDeleteOnTermination = dot
                }
        return InstanceBlockDeviceMapping
            { instanceDeviceName = devname
            , instanceEbs = iebs
            }
    life <- getM "instanceLifecycle" $ const LifeCycleNone
    spotid <- getMT "spotInstanceRequestId"
    vtype <- getF "virtualizationType" t2virtualizationType
    ctoken <- getT "clientToken"
    tset <- resourceTagSink
    hv <- getF "hypervisor" t2hipervisor
    nicset <- networkInterfaceSink
    iam <- element "iamInstanceProfile" $ do
        arn <- getT "arn"
        iamid <- getT "id"
        return IamInstanceProfile
            { iipArn = arn
            , iipId = iamid
            }
    eopt <- getF "ebsOptimized" t2bool
    return Instance
        { instanceId = iid
        , instanceImageId = image
        , instanceState = istate
        , privateDnsName = pdns
        , dnsName = dns
        , reason = res
        , keyName = kname
        , amiLaunchIndex = aidx
        , instanceProductCodes = pcode
        , instanceType = itype
        , launchTime = ltime
        , instancePlacement = placement
        , instanceKernelId = kid
        , instanceRamdiskId = rid
        , instancePlatform = pf
        , monitoring = mon
        , subnetId = snid
        , vpcId = vpcid
        , privateIpAddress = paddr
        , ipAddress = addr
        , sourceDestCheck = sdc
        , vpcGroupSet = group
        , instanceStateReason = sreason
        , instanceArchitecture = arch
        , instanceRootDeviceType = rdtype
        , instanceRootDeviceName = rdname
        , instanceBlockDeviceMappings = bdmap
        , instanceLifecycle = life
        , spotInstanceRequestId = spotid
        , instanceVirtualizationType = vtype
        , clientToken = ctoken
        , instanceTagSet = tset
        , instanceHypervisor = hv
        , instanceNetworkInterfaceSet = nicset
        , iamInstanceProfile = iam
        , ebsOptimized = eopt
        }

networkInterfaceSink :: MonadThrow m
    => GLSink Event m [InstanceNetworkInterface]
networkInterfaceSink = itemsSet "networkInterfaceSet" $ do
    iid <- getT "networkInterfaceId"
    sid <- getT "subnetId"
    vpcid <- getT "vpcId"
    desc <- getM "description" t2emptxt
    own <- getT "ownerId"
    st <- getT "status"
    paddr <- getT "privateIpAddress"
    pdns <- getMT "privateDnsName"
    sdc <- getF "sourceDestCheck" t2bool
    grp <- groupSetSink
    att <- elementF "attachment" $ do
        aid <- getT "attachmentId"
        didx <- getF "deviceIndex" t2dec
        ast <- getT "status"
        atime <- getF "attachTime" t2time
        dot <- getF "deleteOnTermination" t2bool
        return NetworkInterfaceAttachment
            { niatAttachmentId = aid
            , niatDeviceIndex = didx
            , niatStatus = ast
            , niatAttachTime = atime
            , niatDeleteOnTermination = dot
            }
    asso <- niAssociationSink
    pips <- itemsSet "privateIpAddressesSet" $ do
        pip <- getT "privateIpAddress"
        pr <- getF "primary" t2bool
        passo <- niAssociationSink
        return InstancePrivateIpAddress
            { iPrivateIpAddress = pip
            , iPrimary = pr
            , iAssociation = passo
            }
    return InstanceNetworkInterface
        { instanceNetworkInterfaceId = iid
        , iniSubnetId = sid
        , iniVpcId = vpcid
        , iniDescription = desc
        , iniOwnerId = own
        , iniStatus = st
        , iniPrivateIpAddress = paddr
        , iniPrivateDnsName = pdns
        , iniSourceDestCheck = sdc
        , iniGroupSet = grp
        , iniAttachment = att
        , iniAssociation = asso
        , iniPrivateIpAddressSet = pips
        }

niAssociationSink :: MonadThrow m
    => GLSink Event m (Maybe NetworkInterfaceAssociation)
niAssociationSink = element "association" $ do
    aspip <- getT "publicIp"
    asipown <- getT "ipOwnerId"
    return NetworkInterfaceAssociation
        { niasPublicIp = aspip
        , niasIpOwnerId = asipown
        }

