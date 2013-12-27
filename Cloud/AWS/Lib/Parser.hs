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
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.XML.Types (Event)

import Cloud.AWS.Class
import Cloud.AWS.Lib.Parser.Unordered (SimpleXML, getElements, getElement, getElementM, (.<), xmlParser)

type RequestId = Text

fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM a Nothing  = a
fromMaybeM _ (Just a) = return a

sinkResponse
    :: (MonadThrow m, Applicative m)
    => Text -- ^ Action
    -> (SimpleXML -> m a)
    -> Consumer Event m (a, RequestId)
sinkResponse action parser = xmlParser $ \xml ->
    getElement xml (action <> "Response") $ \xml' -> (,)
        <$> (getElementM xml' (action <> "Result") parser >>= fromMaybeM (parser xml')) -- XXX: parse Marker. This marker may not occur (e.g., PutMetricAlarm).
        <*> sinkResponseMetadata xml'

sinkResponseMetadata
    :: (MonadThrow m, Applicative m)
    => SimpleXML -> m Text
sinkResponseMetadata xml =
    getElement xml "ResponseMetadata" (.< "RequestId")

sinkError :: (MonadThrow m, Applicative m)
    => ByteString -> ByteString -> Int -> Consumer Event m a
sinkError region action status = xmlParser $ \sxml -> getElement sxml "ErrorResponse" $ \xml -> do
    (_::Maybe Text,c,m) <- getElement xml "Error" $ \xml' -> (,,)
        <$> xml' .< "Type"
        <*> xml' .< "Code"
        <*> xml' .< "Message"
    rid <- xml .< "RequestId"
    monadThrow $ errorData region action status c m rid
  where
    errorData = if status < 500 then ClientError else ServerError

members :: (MonadThrow m, Applicative m)
    => Text
    -> (SimpleXML -> m a)
    -> SimpleXML
    -> m [a]
members name f xml = getElements xml name "member" f

nodata :: (MonadThrow m, Applicative m)
    => SimpleXML -> m ()
nodata = const $ return ()
