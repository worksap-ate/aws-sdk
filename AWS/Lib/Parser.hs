{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
module AWS.Lib.Parser
    where

import Data.XML.Types (Event(..), Name(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Text.XML.Stream.Parse as XML
import Control.Applicative
import Data.Monoid
import Control.Monad.Trans.Class (lift)
import Safe (readMay)

import AWS.Class

type RequestId = Text

text :: MonadThrow m => GLSink Event m Text
text = XML.content

listConduit :: MonadThrow m
    => Text
    -> GLSink Event m o
    -> GLConduit Event m o
listConduit name p =
    awaitWhile isTag >>= maybe (return ()) (\e -> do
        leftover e
        if isBeginTagName name e
            then do
                element name $ p >>= yield
                listConduit name p
            else return ()
        )

listConsumer :: MonadThrow m
    => Text
    -> GLSink Event m o
    -> GLSink Event m [o]
listConsumer name p = listConduit name p >+> CL.consume

isTag :: Event -> Bool
isTag (EventBeginElement _ _) =True
isTag (EventEndElement _) =True
isTag _ = False

sinkDropWhile :: Monad m => (i -> Bool) -> GLSink i m ()
sinkDropWhile f = await >>= maybe (return ()) g
  where
      g i | f i       = sinkDropWhile f
              | otherwise = leftover i >> return ()

isBeginTagName :: Text -> Event -> Bool
isBeginTagName name (EventBeginElement n _)
    | nameLocalName n == name = True
    | otherwise               = False
isBeginTagName _ _ = False

awaitWhile :: Monad m
    => (i -> Bool)
    -> Pipe l i o u m (Maybe i)
awaitWhile f = await >>= g
  where
    g Nothing       = return Nothing
    g (Just a)
        | f a       = return $ Just a
        | otherwise = awaitWhile f

getF :: MonadThrow m
    => Text
    -> (Text -> b)
    -> Pipe Event Event o u m b
getF name f = f <$> tagContent name

textReadMay :: forall a . Read a => Text -> Maybe a
textReadMay t
    = maybe (readMay $ T.unpack t) Just
    . readMay
    . show
    $ t

textReadM :: (Read a, MonadThrow m) => Text -> m a
textReadM t = maybe
    (monadThrow $ TextConversionException t)
    return
    $ textReadMay t

getT :: (MonadThrow m, Read a, Show a)
    => Text
    -> Pipe Event Event o u m a
getT name = tagContent name >>= lift . textReadM

getT_ :: forall m o u . MonadThrow m
    => Text
    -> Pipe Event Event o u m ()
getT_ name = () <$ (getT name :: Pipe Event Event o u m Text)

getM :: MonadThrow m
    => Text
    -> (Maybe Text -> b)
    -> Pipe Event Event o u m b
getM name f = f <$> tagContentM name

getMT :: (MonadThrow m, Read a)
    => Text
    -> Pipe Event Event o u m (Maybe a)
getMT name = getM name (>>= textReadMay)

elementM :: MonadThrow m
    => Text
    -> Pipe Event Event o u m a
    -> Pipe Event Event o u m (Maybe a)
elementM name inner = do
    sinkDropWhile $ not . isTag
    XML.tagPredicate g (return ()) $ const inner
  where
    g n = (nameLocalName n == name)

element :: MonadThrow m
    => Text
    -> Pipe Event Event o u m a
    -> Pipe Event Event o u m a
element name inner = XML.force ("parse error:" ++ T.unpack name) $ elementM name inner

tagContentM :: MonadThrow m
    => Text
    -> GLSink Event m (Maybe Text)
tagContentM name = elementM name text

tagContent :: MonadThrow m
    => Text
    -> GLSink Event m Text
tagContent name =
    XML.force ("parse error:" ++ T.unpack name) $ tagContentM name

sinkResponse
    :: MonadThrow m
    => Text -- ^ Action
    -> GLSink Event m a
    -> GLSink Event m (a, RequestId)
sinkResponse action sink = do
    sinkEventBeginDocument
    element (action <> "Response") $ (,)
        <$> element (action <> "Result") sink -- XXX: parse Marker
        <*> sinkResponseMetadata

sinkResponseMetadata
    :: MonadThrow m
    => GLSink Event m Text
sinkResponseMetadata =
    element "ResponseMetadata" $
        getT "RequestId"

sinkEventBeginDocument
    :: MonadThrow m
    => GLSink Event m ()
sinkEventBeginDocument = do
    me <- await
    case me of
        Nothing -> return ()
        Just EventBeginDocument -> return ()
        Just _ -> fail $ "unexpected: " <> show me

sinkError :: MonadThrow m => ByteString -> Int -> GLSink Event m a
sinkError action status = element "ErrorResponse" $ do
    (c,m) <- element "Error" $ (,)
        <$> (getT_ "Type" *> getT "Code")
        <*> getT "Message"
    rid <- getT "RequestId"
    lift $ monadThrow $ ClientError action status c m rid

members :: MonadThrow m
    => Text
    -> GLSink Event m a
    -> GLSink Event m [a]
members name f = element name $ listConsumer "member" f
