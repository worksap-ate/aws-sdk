{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Cloud.AWS.Lib.Parser
    ( RequestId
    , sinkResponse
    , sinkResponseMetadata
    , sinkError
    , members
    , nodata
    ) where

import Control.Applicative
import Control.Monad.Trans (lift)
import Data.ByteString (ByteString)
import Data.Conduit
import Control.Monad.Trans.Resource (MonadThrow)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.XML.Types (Event)

import Cloud.AWS.Lib.Parser.Unordered
    (XmlElement, elements, element, elementM, (.<), elementConsumer)

import Cloud.AWS.Class

type RequestId = Text

fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM a Nothing  = a
fromMaybeM _ (Just a) = return a

sinkResponse
    :: (MonadThrow m, Applicative m)
    => Text -- ^ Action
    -> (XmlElement -> m a)
    -> Consumer Event m (a, RequestId)
sinkResponse action conv =
    elementConsumer >>= lift . element (action <> "Response") conv'
  where
    conv' e = (,)
        <$> (elementM (action <> "Result") conv e >>= fromMaybeM (conv e)) -- XXX: parse Marker. This marker may not occur (e.g., PutMetricAlarm).
        <*> sinkResponseMetadata e

sinkResponseMetadata
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m Text
sinkResponseMetadata = element "ResponseMetadata" (.< "RequestId")

sinkError :: (MonadThrow m, Applicative m)
    => ByteString -> ByteString -> Int -> Consumer Event m a
sinkError region action status = elementConsumer >>= element "ErrorResponse" conv
  where
    conv e = do
        (_::Maybe Text,c,m) <- element "Error" tupleConv e
        rid <- e .< "RequestId"
        monadThrow $ errorData region action status c m rid
    tupleConv e = (,,)
        <$> e .< "Type"
        <*> e .< "Code"
        <*> e .< "Message"
    errorData = if status < 500 then ClientError else ServerError

members :: (MonadThrow m, Applicative m)
    => Text
    -> (XmlElement -> m a)
    -> XmlElement
    -> m [a]
members name = elements name "member"

nodata :: (MonadThrow m, Applicative m)
    => XmlElement -> m ()
nodata = const $ return ()
