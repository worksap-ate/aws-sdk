{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module AWS.Lib.Parser
    where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Lazy.Char8 ()

import Data.XML.Types (Event(..), Name(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Text.XML.Stream.Parse as XML
import Control.Applicative
import Data.Monoid ((<>))

apiVersion :: ByteString
apiVersion = "2012-08-15"

itemConduit :: MonadThrow m
    => Text
    -> GLSink Event m o
    -> GLConduit Event m o
itemConduit tag inner = do
    maybe (()) id <$> elementM tag (items inner)
  where
    items :: MonadThrow m
        => Pipe Event Event o u m o
        -> Pipe Event Event o u m ()
    items p = awaitWhile isTag >>= maybe (return ()) (\e -> do
        leftover e
        if isBeginTagName "item" e
            then do
                element "item" $ p >>= yield
                items p
            else return ()
        )

itemsSet :: MonadThrow m
    => Text
    -> GLSink Event m o
    -> GLSink Event m [o]
itemsSet tag inner = itemConduit tag inner >+> CL.consume

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

elementM :: MonadThrow m
    => Text
    -> Pipe Event Event o u m a
    -> Pipe Event Event o u m (Maybe a)
elementM name inner = XML.tagNoAttr (ec2Name name) inner

element :: MonadThrow m
    => Text
    -> Pipe Event Event o u m a
    -> Pipe Event Event o u m a
element name inner = XML.force "parse error" $ elementM name inner

tagContent :: MonadThrow m
    => Text
    -> GLSink Event m (Maybe Text)
tagContent name = XML.tagNoAttr (ec2Name name) XML.content

tagContentF :: MonadThrow m
    => Text
    -> GLSink Event m Text
tagContentF = XML.force "parse error" . tagContent

ec2Name :: Text -> Name
ec2Name name = Name
    { nameLocalName = name
    , nameNamespace =
        Just $ "http://ec2.amazonaws.com/doc/" <> T.pack (BSC.unpack apiVersion) <> "/"
    , namePrefix = Nothing
    }
