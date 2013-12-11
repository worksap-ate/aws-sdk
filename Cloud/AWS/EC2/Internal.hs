{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cloud.AWS.EC2.Internal
    ( module Cloud.AWS.Class
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
      -- new parsers
    , itemConduit'
    , itemsSet'
    , resourceTagConv
    , productCodeConv
    , stateReasonConv
    , volumeTypeConv
    ) where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import qualified Network.HTTP.Conduit as HTTP
import Data.ByteString.Char8 ()
import Control.Applicative
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe (fromMaybe)
import Data.XML.Types (Event)
import Data.Text (Text)

import Cloud.AWS.Class
import Cloud.AWS.Lib.Parser
import Cloud.AWS.Lib.Parser.Unordered hiding (getT)
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

itemConduit :: MonadThrow m
    => Text
    -> Consumer Event m o
    -> Conduit Event m o
itemConduit tag inner =
    fromMaybe (()) <$> elementM tag (listConduit "item" inner)

itemConduit' :: (MonadThrow m, Applicative m)
    => Text
    -> (SimpleXML -> m o)
    -> Conduit Event m o
itemConduit' tag inner = xmlParserConduit tag $ \xml ->
    getElement xml "item" inner

itemsSet :: MonadThrow m
    => Text
    -> Consumer Event m o
    -> Consumer Event m [o]
itemsSet tag inner = itemConduit tag inner =$= CL.consume

itemsSet' :: (MonadThrow m, Applicative m)
    => SimpleXML
    -> Text
    -> (SimpleXML -> m o)
    -> m [o]
itemsSet' xml tag inner = getElements xml tag "item" inner

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

-- new parsers

resourceTagConv :: (MonadThrow m, Applicative m)
    => SimpleXML -> m [ResourceTag]
resourceTagConv xml = getElements xml "tagSet" "item" $ \xml' ->
    ResourceTag
    <$> xml' .< "key"
    <*> xml' .< "value"

productCodeConv :: (MonadThrow m, Applicative m)
    => SimpleXML -> m [ProductCode]
productCodeConv xml = itemsSet' xml "productCodes" $ \xml' ->
    ProductCode
    <$> xml' .< "productCode"
    <*> xml' .< "type"

stateReasonConv :: (MonadThrow m, Applicative m)
    => SimpleXML -> m (Maybe StateReason)
stateReasonConv xml = getElementM xml "stateReason" $ \xml' ->
    StateReason
    <$> xml' .< "code"
    <*> xml' .< "message"

volumeTypeConv :: (MonadThrow m, Applicative m)
    => SimpleXML -> m VolumeType
volumeTypeConv xml = join $ volumeType
    <$> xml .< "volumeType"
    <*> xml .< "iops"
