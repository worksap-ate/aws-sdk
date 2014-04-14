{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Cloud.AWS.EC2.Internal
    ( module Cloud.AWS.Class
    , EC2
    , initialEC2Context
    , runEC2
    , runEC2withManager
    , itemConduit
    , itemsSet
    , itemsPath
    , resourceTagConv
    , productCodeConv
    , stateReasonConv
    , volumeTypeConv
    , groupSetConv
    , networkInterfaceAttachmentConv
    ) where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource (MonadThrow, MonadBaseControl)
import qualified Network.HTTP.Conduit as HTTP
import Data.ByteString.Char8 ()
import Control.Applicative
import Data.Conduit
import Data.Text (Text)

import Cloud.AWS.Lib.Parser.Unordered (XmlElement, (.<), convertConduit, element, elements, ElementPath, tag, (.-), elementM, end)

import Cloud.AWS.Class
import Cloud.AWS.EC2.Types

initialEC2Context :: HTTP.Manager -> AWSContext
initialEC2Context mgr = AWSContext
    { manager = mgr
    , endpoint = "ec2.amazonaws.com"
    , lastRequestId = Nothing
    }

type EC2 m a = AWS AWSContext m a

runEC2 :: MonadIO m => AWS AWSContext m a -> m a
runEC2 = runAWS initialEC2Context

runEC2withManager :: Monad m
    => HTTP.Manager -> AWSSettings -> AWS AWSContext m a -> m a
runEC2withManager mgr =
    runAWSwithManager mgr initialEC2Context

itemConduit :: (MonadBaseControl IO m, MonadThrow m)
    => (XmlElement -> m o)
    -> Conduit XmlElement m o
itemConduit inner = convertConduit $ element "item" inner

itemsSet :: MonadThrow m
    => Text
    -> (XmlElement -> m o)
    -> XmlElement
    -> m [o]
itemsSet t = elements t "item"

itemsPath :: Text -> ElementPath
itemsPath t = tag t .- end "item"

volumeType :: MonadThrow m => Text -> Maybe Int -> m VolumeType
volumeType t Nothing  | t == "standard" = return $ VolumeTypeStandard
volumeType t (Just i) | t == "io1"      = return $ VolumeTypeIO1 i
volumeType t _ = monadThrow $ FromTextError t

resourceTagConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m [ResourceTag]
resourceTagConv = elements "tagSet" "item" conv
  where
    conv e = ResourceTag
        <$> e .< "key"
        <*> e .< "value"

productCodeConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m [ProductCode]
productCodeConv = itemsSet "productCodes" conv
  where
    conv e = ProductCode
        <$> e .< "productCode"
        <*> e .< "type"

stateReasonConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m (Maybe StateReason)
stateReasonConv = elementM "stateReason" conv
  where
    conv e = StateReason
        <$> e .< "code"
        <*> e .< "message"

volumeTypeConv :: (MonadThrow m, Applicative m)
    => XmlElement -> m VolumeType
volumeTypeConv xml = join $ volumeType
    <$> xml .< "volumeType"
    <*> xml .< "iops"

groupSetConv :: (MonadThrow m, Applicative m) => XmlElement -> m [Group]
groupSetConv = itemsSet "groupSet" conv
  where
    conv e = Group
        <$> e .< "groupId"
        <*> e .< "groupName"

networkInterfaceAttachmentConv
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m (Maybe NetworkInterfaceAttachment)
networkInterfaceAttachmentConv = elementM "attachment" conv
  where
    conv e = NetworkInterfaceAttachment
        <$> e .< "attachmentId"
        <*> e .< "instanceId"
        <*> e .< "instanceOwnerId"
        <*> e .< "deviceIndex"
        <*> e .< "status"
        <*> e .< "attachTime"
        <*> e .< "deleteOnTermination"
