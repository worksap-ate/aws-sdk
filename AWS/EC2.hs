{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2
    ( module AWS.EC2.Types
    , EC2Endpoint(..)
    , EC2
    , EC2Context
    , newEC2Context
    , runEC2
    , describeImages
    , describeRegions
    , describeAvailabilityZones
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
import Text.XML.Stream.Parse
import Safe
import Control.Monad.State

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
    -> EC2 m (EC2Response (Source m Image))
describeImages imageIds =
    ec2Query "DescribeImages" params imagesSetConduit
  where
    params = [ArrayParams "ImageId" imageIds]

imagesSetConduit :: MonadThrow m
    => GLConduit Event m Image
imagesSetConduit = "imagesSet" >< items imageItem

items :: MonadThrow m
    => Pipe Event Event o u m o
    -> Pipe Event Event o u m ()
items p = do
    awaitWhile isTag >>= maybe (return ()) (\e -> do
        leftover e
        if isBeginTagName "item" e
            then do
                "item" >< p >>= yield
                items p
            else return ()
        )

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
    pc <- productCodeConduit >+> CL.consume
    a <- getT "architecture"
    t <- getF "imageType" t2imageType
    kid <- getMT "kernelId"
    rid <- getMT "ramdiskId"
    pf <- getM "platform" t2platform
    sr <- stateReasonSink
    oa <- getMT "imageOwnerAlias"
    n <- getMT "name"
    d <- getMT "description"
    rdt <- getF "rootDeviceType" t2rootDeviceType
    rdn <- getMT "rootDeviceName"
    bdms <- blockDeviceMapping >+> CL.consume
    vt <- getF "virtualizationType" t2virtualizationType
    ts <- resourceTagConduit >+> CL.consume
    h <- getF "hypervisor" t2hipervisor
    return $ Image
        { imageId = i
        , imageLocation = l
        , imageState = s
        , imageOwnerId = oid
        , isPublic = p
        , productCodes = pc
        , architecture = a
        , imageType = t
        , kernelId = kid
        , ramdiskId = rid
        , platform = pf
        , stateReason = sr
        , imageOwnerAlias = oa
        , imageName = n
        , description = d
        , rootDeviceType = rdt
        , rootDeviceName = rdn
        , blockDeviceMappings = bdms
        , virtualizationType = vt
        , tagSet = ts
        , hipervisor = h
        }

blockDeviceMapping :: MonadThrow m
    => GLConduit Event m BlockDeviceMapping
blockDeviceMapping = do
    "blockDeviceMapping" >|< items $ do
        n <- getT "deviceName"
        v <- getMT "virutalName"
        e <- ebsParser
        return $ BlockDeviceMapping
            { deviceName = n
            , virtualName = v
            , ebs = e
            }
    return ()
  where
    ebsParser :: MonadThrow m
        => Pipe Event Event o u m (Maybe EbsBlockDevice)
    ebsParser = "ebs" >|< do
        sid <- getMT "snapshotId"
        vs <- getF "volumeSize" t2dec
        dot <- getF "deleteOnTermination" t2bool
        vt <- getF "volumeType" t2volumeType
        io <- getM "iops" t2iops
        return $ EbsBlockDevice
            { snapshotId = sid
            , volumeSize = vs
            , deleteOnTermination = dot
            , volumeType = vt
            , iops = io
            }

resourceTagConduit :: MonadThrow m
    => GLConduit Event m ResourceTag
resourceTagConduit = do
    "tagSet" >|< items $ do
        k <- getT "key"
        v <- getT "value"
        return ResourceTag
            { resourceKey = k
            , resourceValue = v
            }
    return ()

productCodeConduit :: MonadThrow m
    => GLConduit Event m ProductCode
productCodeConduit = do
    "productCodes" >|< items $ do
        c <- getT "productCode"
        t <- getF "type" t2productCodeType
        return ProductCode
            { productCode = c
            , productCodeType = t
            }
    return ()

stateReasonSink :: MonadThrow m
    => Pipe Event Event o u m (Maybe StateReason)
stateReasonSink = do
    msr <- "stateReason" >|< do
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

infixr 0 ><
(><) :: MonadThrow m
    => Text
    -> Pipe Event Event o u m a
    -> Pipe Event Event o u m a
name >< inner = elementF name inner

infixr 0 >|<
(>|<) :: MonadThrow m
    => Text
    -> Pipe Event Event o u m a
    -> Pipe Event Event o u m (Maybe a)
name >|< inner = element name inner

element :: MonadThrow m
    => Text
    -> Pipe Event Event o u m a
    -> Pipe Event Event o u m (Maybe a)
element name inner = tagNoAttr (ec2Name name) inner

elementF :: MonadThrow m
    => Text
    -> Pipe Event Event o u m a
    -> Pipe Event Event o u m a
elementF name inner = force "parse error" $ element name inner

err :: String -> Text -> a
err v m = error $ "unknown " ++ v ++ ": " ++ T.unpack m

t2imageState :: Text -> ImageState
t2imageState a
    | a == "available" = Available
    | a == "pending"   = Pending
    | a == "failed"    = Failed
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


{----------------------------------------------------
 - DescribeRegions
 ---------------------------------------------------}
describeRegions
    :: (MonadResource m, MonadBaseControl IO m)
    => [ByteString]
    -> EC2 m (EC2Response (Source m Region))
describeRegions regions =
    ec2Query "DescribeRegions" params regionInfoConduit
  where
    params = [ArrayParams "RegionName" regions]

    regionInfoConduit :: MonadThrow m
        => GLConduit Event m Region
    regionInfoConduit = "regionInfo" >< items $ do
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
    -> EC2 m (EC2Response (Source m AvailabilityZone))
describeAvailabilityZones zones =
    ec2Query "DescribeAvailabilityZones" params availabilityZoneInfo
  where
    params = [ArrayParams "ZoneName" zones]

    availabilityZoneInfo :: MonadThrow m
        => GLConduit Event m AvailabilityZone
    availabilityZoneInfo =
        "availabilityZoneInfo" >< items $ do
            name <- getT "zoneName"
            st <- getT "zoneState"
            region <- getT "regionName"
            msgs <- zoneMessageSet >+> CL.consume
            return AvailabilityZone
                { zoneName = name
                , zoneState = st
                , zoneRegionName = region
                , messageSet = msgs
                }

    zoneMessageSet :: MonadThrow m
        => GLConduit Event m AvailabilityZoneMessage
    zoneMessageSet = do
        "messageSet" >|< items $ getT "message"
        return ()

