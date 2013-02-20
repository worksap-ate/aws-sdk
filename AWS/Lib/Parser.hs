{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module AWS.Lib.Parser
    ( RequestId
    , whenM
    , getT
    , getT_
    , element
    , elementM
    , listConduit
    , listConsumer
    , isBeginTagName
    , awaitWhile
    , sinkResponse
    , sinkResponseMetadata
    , sinkError
    , sinkEventBeginDocument
    , members
    , text
    , FromText(..)
    ) where

import Data.XML.Types (Event(..), Name(..))
import Data.ByteString (ByteString)
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Text.XML.Stream.Parse as XML
import Control.Applicative
import Control.Monad (when, void)
import Data.Monoid ((<>))
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe)

import AWS.Class
import AWS.Lib.FromText

type RequestId = Text

text :: MonadThrow m => Consumer Event m Text
text = XML.content

whenM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenM mma f = mma >>= maybe (return ()) f

fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM a Nothing  = a
fromMaybeM _ (Just a) = return a

listConduit :: MonadThrow m
    => Text
    -> Consumer Event m o
    -> Conduit Event m o
listConduit name p = whenM (awaitWhile isTag) $ \e -> do
    leftover e
    when (isBeginTagName name e) $
        whenM (elementM name p) $ \a -> do
            yield a
            listConduit name p

listConsumer :: MonadThrow m
    => Text
    -> Consumer Event m o
    -> Consumer Event m [o]
listConsumer name p = listConduit name p =$= CL.consume

isTag :: Event -> Bool
isTag (EventBeginElement _ _) =True
isTag (EventEndElement _) =True
isTag _ = False

sinkDropWhile :: Monad m => (i -> Bool) -> Consumer i m ()
sinkDropWhile f = whenM await g
  where
    g i | f i       = sinkDropWhile f
        | otherwise = void $ leftover i

isBeginTagName :: Text -> Event -> Bool
isBeginTagName name (EventBeginElement n _)
    | nameLocalName n == name = True
    | otherwise               = False
isBeginTagName _ _ = False

awaitWhile :: Monad m
    => (i -> Bool)
    -> Consumer i m (Maybe i)
awaitWhile f = await >>= g
  where
    g Nothing       = return Nothing
    g (Just a)
        | f a       = return $ Just a
        | otherwise = awaitWhile f

getT :: (MonadThrow m, FromText a)
    => Text
    -> Consumer Event m a
getT name = elementM name text >>= lift . fromMaybeText name

getT_ :: forall m . MonadThrow m
    => Text
    -> Consumer Event m ()
getT_ name = () <$ (getT name :: Consumer Event m (Maybe Text))

elementM :: forall m a . MonadThrow m
    => Text
    -> Consumer Event m a
    -> Consumer Event m (Maybe a)
elementM name inner = do
    sinkDropWhile $ not . isTag
    XML.tagPredicate g (return ()) $ const inner
  where
    g n = (nameLocalName n == name)

element :: forall m a . MonadThrow m
    => Text
    -> Consumer Event m a
    -> Consumer Event m a
element name inner = elementM name inner >>=
    fromMaybeM (lift $ monadThrow $ ResponseParseError name)

sinkResponse
    :: MonadThrow m
    => Text -- ^ Action
    -> Consumer Event m a
    -> Consumer Event m (a, RequestId)
sinkResponse action sink = do
    sinkEventBeginDocument
    element (action <> "Response") $ (,)
        <$> sinkResult
        <*> sinkResponseMetadata
  where
    sinkResult =
        elementM (action <> "Result") sink -- XXX: parse Marker. This marker may not occur (e.g., PutMetricAlarm).
        >>= fromMaybeM sink

sinkResponseMetadata
    :: MonadThrow m
    => Consumer Event m Text
sinkResponseMetadata =
    element "ResponseMetadata" $
        getT "RequestId"

sinkEventBeginDocument
    :: MonadThrow m
    => Consumer Event m ()
sinkEventBeginDocument = do
    me <- await
    case me of
        Nothing -> return ()
        Just EventBeginDocument -> return ()
        Just _ -> fail $ "unexpected: " <> show me

sinkError :: MonadThrow m => ByteString -> Int -> Consumer Event m a
sinkError action status = element "ErrorResponse" $ do
    (c,m) <- element "Error" $ (,)
        <$> (getT_ "Type" *> getT "Code")
        <*> getT "Message"
    rid <- getT "RequestId"
    lift $ monadThrow $ ClientError action status c m rid

members :: MonadThrow m
    => Text
    -> Consumer Event m a
    -> Consumer Event m [a]
members name f =
    fromMaybe [] <$> elementM name (listConsumer "member" f)
