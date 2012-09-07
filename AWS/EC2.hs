{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2
    ( module AWS.EC2.Types
    , Ec2Endpoint(..)
    , queryStr
    , params
    , wrap
    , describeImages
    ) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Word (Word8)
import           Data.Monoid

import qualified Codec.Binary.Url as Url

import Data.XML.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Control
import Network.HTTP.Conduit
import Text.XML.Stream.Parse
import Safe

import AWS.EC2.Types

wrap :: ([Word8] -> String) -> ByteString -> ByteString
wrap f = LC.pack . f . L.unpack

type Param = (ByteString, ByteString)

data Ec2Endpoint = UsEast1
                 | ApNortheast1

params :: ByteString -> ByteString -> [Param]
params a t =
    [ ("Action", "DescribeImages")
    , ("ImageId.1", "ami-e565ba8c")
    , ("ImageId.2", "ami-6678da0f")
    , ("ImageId.3", "ami-0067ca69")
    , ("ImageId.4", "ami-f21aff9b")
    , ("ImageId.5", "ami-03d37c6a")
    , ("ImageId.6", "aki-f5c1219c")
    , ("ImageId.7", "ari-f606f39f")
    , ("ImageId.8", "ami-8a3cc9e3")
    , ("Timestamp", wrap Url.encode t)
    , ("Version", "2012-07-20")
    , ("SignatureVersion", "2")
    , ("SignatureMethod", "HmacSHA256")
    , ("AWSAccessKeyId", a)
    ]

queryStr :: [Param] -> ByteString
queryStr = join "&" . map param
  where
    param :: Param -> ByteString
    param (k, v) = mconcat [k, "=", v]
    join :: ByteString -> [ByteString] -> ByteString
    join sep = foldl1 $ \a b -> mconcat [a, sep, b]

describeImages 
    :: (MonadResource m, MonadBaseControl IO m)
    => Request m
    -> Manager
    -> m (DescribeImagesResponse (Source m Image))
describeImages request manager = do
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

