{-# LANGUAGE RankNTypes, ScopedTypeVariables, CPP #-}

module AWS.EC2.Internal
    ( module AWS.Class
    , EC2
    , initialEC2Context
    , runEC2
    , runEC2withManager
    , itemConduit
    , itemsSet
    , resourceTagSink
    , productCodeSink
    , stateReasonSink
    , volumeTypeSink
    , groupSetSink
    , networkInterfaceAttachmentSink
    ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadThrow)
#endif
import qualified Network.HTTP.Conduit as HTTP
import Data.ByteString.Char8 ()
import Control.Applicative
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe (fromMaybe)
import Data.XML.Types (Event)
import Data.Text (Text)

import AWS.Class
import AWS.Credential
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

runEC2withManager :: Monad m
    => HTTP.Manager -> Credential -> AWS AWSContext m a -> m a
runEC2withManager mgr =
    runAWSwithManager mgr initialEC2Context

itemConduit :: MonadThrow m
    => Text
    -> Consumer Event m o
    -> Conduit Event m o
itemConduit tag inner =
    fromMaybe (()) <$> elementM tag (listConduit "item" inner)

itemsSet :: MonadThrow m
    => Text
    -> Consumer Event m o
    -> Consumer Event m [o]
itemsSet tag inner = itemConduit tag inner =$= CL.consume

resourceTagSink :: MonadThrow m
    => Consumer Event m [ResourceTag]
resourceTagSink = itemsSet "tagSet" $
    ResourceTag
    <$> getT "key"
    <*> getT "value"

productCodeSink :: MonadThrow m
    => Consumer Event m [ProductCode]
productCodeSink = itemsSet "productCodes" $
    ProductCode
    <$> getT "productCode"
    <*> getT "type"

stateReasonSink :: MonadThrow m
    => Consumer Event m (Maybe StateReason)
stateReasonSink = elementM "stateReason" $
    StateReason
    <$> getT "code"
    <*> getT "message"

volumeType :: MonadThrow m => Text -> Maybe Int -> m VolumeType
volumeType t Nothing  | t == "standard" = return $ VolumeTypeStandard
volumeType t (Just i) | t == "io1"      = return $ VolumeTypeIO1 i
volumeType t _ = monadThrow $ FromTextError t

volumeTypeSink :: MonadThrow m
    => Consumer Event m VolumeType
volumeTypeSink = volumeType <$> getT "volumeType" <*> getT "iops" >>= lift

groupSetSink :: MonadThrow m => Consumer Event m [Group]
groupSetSink = itemsSet "groupSet" $ Group
    <$> getT "groupId"
    <*> getT "groupName"

networkInterfaceAttachmentSink
    :: MonadThrow m
    => Consumer Event m (Maybe NetworkInterfaceAttachment)
networkInterfaceAttachmentSink = elementM "attachment" $
    NetworkInterfaceAttachment
    <$> getT "attachmentId"
    <*> getT "instanceId"
    <*> getT "instanceOwnerId"
    <*> getT "deviceIndex"
    <*> getT "status"
    <*> getT "attachTime"
    <*> getT "deleteOnTermination"
