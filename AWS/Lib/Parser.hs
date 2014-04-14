{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

module AWS.Lib.Parser
    ( RequestId
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
import Data.Char (isSpace)
import Data.Conduit
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadThrow)
#endif
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Text.XML.Stream.Parse as XML
import Data.XML.Types (Content(..))
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

elementM :: MonadThrow m
    => Text
    -> ConduitM Event o m a
    -> ConduitM Event o m (Maybe a)
elementM name inner = do
    sinkDropWhile $ not . isTag
    tagConduitM g inner
  where
    g n = nameLocalName n == name

element :: MonadThrow m
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
    lift $ monadThrow $ errorData action status c m rid
  where
    errorData = if status < 500 then ClientError else ServerError

members :: MonadThrow m
    => Text
    -> Consumer Event m a
    -> Consumer Event m [a]
members name f =
    fromMaybe [] <$> elementM name (listConsumer "member" f)

-- | Text.XML.Stream.Parse.tag using ConduitM
tagConduitM :: MonadThrow m
    => (Name -> Bool)
    -> ConduitM Event o m c
    -> ConduitM Event o m (Maybe c)
tagConduitM checkName inner = do
    x <- dropWS
    case x of
        Just (EventBeginElement name _) ->
            if checkName name
                then do
                    CL.drop 1
                    z' <- inner
                    a <- dropWS
                    case a of
                        Just (EventEndElement name')
                            | name == name' -> CL.drop 1 >> return (Just z')
                        _ -> lift $ monadThrow $ XML.XmlException ("Expected end tag for: " ++ show name) a
                else return Nothing
        _ -> return Nothing
  where
    dropWS = do
        x <- CL.peek
        let isWS =
                case x of
                    Just EventBeginDocument -> True
                    Just EventEndDocument -> True
                    Just EventBeginDoctype{} -> True
                    Just EventEndDoctype -> True
                    Just EventInstruction{} -> True
                    Just EventBeginElement{} -> False
                    Just EventEndElement{} -> False
                    Just (EventContent (ContentText t))
                        | T.all isSpace t -> True
                        | otherwise -> False
                    Just (EventContent ContentEntity{}) -> False
                    Just EventComment{} -> True
                    Just EventCDATA{} -> False
                    Nothing -> False
        if isWS then CL.drop 1 >> dropWS else return x
