{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2
    ( module AWS.EC2.Types
    , Ec2Endpoint(..)
    , mkUrl
    , describeImages
    ) where

import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Char8 as BSC

import Data.Monoid
import Data.XML.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Control
import Network.HTTP.Conduit
import Text.XML.Stream.Parse
import Safe
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Map.Strict as Map

import AWS
import AWS.Types
import AWS.EC2.Types

params :: QueryParams
params = Map.fromList
    [ ("Action", "DescribeImages")
    , ("Version", "2012-07-20")
    , ("SignatureVersion", "2")
    , ("SignatureMethod", "HmacSHA256")
    ]

mkUrl :: Endpoint end
      => end -> Credential -> UTCTime -> [ByteString] -> ByteString
mkUrl endpoint cred time imageIds = mkUrl' endpoint cred time pars
  where
    pars = Map.union params $ Map.fromList
        [("ImageId." `mappend` (BSC.pack $ show i), imgId) | (i, imgId) <- zip [1..] imageIds]

describeImages 
    :: (MonadResource m, MonadBaseControl IO m, Endpoint end)
    => Manager
    -> Credential
    -> end
    -> [ByteString]
    -> m (DescribeImagesResponse (Source m Image))
describeImages manager cred endpoint imageIds = do
    time <- liftIO getCurrentTime
    let url = mkUrl endpoint cred time imageIds
    request <- liftIO $ parseUrl (BSC.unpack url)
    response <- http request manager
    (res, _) <- unwrapResumable $ responseBody response
    (src, rid) <- res $= parseBytes def
        $$+ describeImagesResponseHeader
    (src1, _) <- unwrapResumable src
    return $ DescribeImagesResponse
        { requestId = rid
        , imagesSet = src1 $= imagesSetConduit
        }

describeImagesResponseHeader :: MonadThrow m
    => Pipe Event Event o u m Text
describeImagesResponseHeader = do
    await -- EventBeginDocument
    await -- EventBeginElement DescribeImagesResponse
    tagContentF "requestId"

imagesSetConduit :: MonadThrow m
    => Pipe Event Event Image u m ()
imagesSetConduit = elementF "imagesSet" $ items imageItem

items :: MonadThrow m
    => Pipe Event Event o u m o
    -> Pipe Event Event o u m ()
items p = do
    awaitWhile isTag >>= maybe (return ()) (\e -> do
        leftover e
        if isBeginTagName "item" e
            then do
                p >>= yield
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
    => Pipe Event Event o u m Image
imageItem = elementF "item" $ do
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
    => Pipe Event Event BlockDeviceMapping u m ()
blockDeviceMapping = do
    element "blockDeviceMapping" $ items blockDeviceMappingItem
    return ()
  where
    blockDeviceMappingItem :: MonadThrow m
        => Pipe Event Event o u m BlockDeviceMapping
    blockDeviceMappingItem = elementF "item" $ do
        n <- getT "deviceName"
        v <- getMT "virutalName"
        e <- ebsParser
        return $ BlockDeviceMapping
            { deviceName = n
            , virtualName = v
            , ebs = e
            }

    ebsParser :: MonadThrow m
        => Pipe Event Event o u m (Maybe EbsBlockDevice)
    ebsParser = element "ebs" $ do
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
    => Pipe Event Event ResourceTag u m ()
resourceTagConduit = do
    element "tagSet" $ items tagSetsItem
    return ()
  where
    tagSetsItem = elementF "item" $ do
        k <- getT "key"
        v <- getT "value"
        return ResourceTag
            { resourceKey = k
            , resourceValue = v
            }

productCodeConduit :: MonadThrow m
    => Pipe Event Event ProductCode u m ()
productCodeConduit = do
    element "productCodes" $ items pcItem
    return ()
  where
    pcItem = elementF "item" $ do
        c <- getT "productCode"
        t <- getF "type" t2productCodeType
        return ProductCode
            { productCode = c
            , productCodeType = t
            }

stateReasonSink :: MonadThrow m
    => Pipe Event Event o u m (Maybe StateReason)
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
element name inner = tagNoAttr (ec2Name name) inner

elementF :: MonadThrow m
    => Text
    -> Pipe Event Event o u m a
    -> Pipe Event Event o u m a
elementF name inner = force "parse error" $ element name inner

tagContent :: MonadThrow m
    => Text
    -> Pipe Event Event o u m (Maybe Text)
tagContent name = tagNoAttr (ec2Name name) content

tagContentF :: MonadThrow m
    => Text
    -> Pipe Event Event o u m Text
tagContentF = force "parse error" . tagContent

ec2Name :: Text -> Name
ec2Name name = Name
    { nameLocalName = name
    , nameNamespace =
        Just "http://ec2.amazonaws.com/doc/2012-07-20/"
    , namePrefix = Nothing
    }

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

