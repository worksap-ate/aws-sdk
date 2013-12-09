{-# LANGUAGE FlexibleContexts, RankNTypes, RecordWildCards #-}

module Cloud.AWS.CloudWatch.Internal
    where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Conduit
import Data.Monoid ((<>))
import Data.XML.Types (Event(..))

import Cloud.AWS.Class
import Cloud.AWS.Lib.Query
import Cloud.AWS.Lib.Parser
import Cloud.AWS.Lib.Parser.Unordered hiding (getT)
import Cloud.AWS.CloudWatch.Types

-- | Ver.2010-08-01
apiVersion :: ByteString
apiVersion = "2010-08-01"

type CloudWatch m a = AWS AWSContext m a

cloudWatchQuery
    :: (MonadBaseControl IO m, MonadResource m)
    => ByteString -- ^ Action
    -> [QueryParam]
    -> Consumer Event m a
    -> CloudWatch m a
cloudWatchQuery = commonQuery apiVersion

elements :: MonadThrow m
    => Text
    -> Consumer Event m a
    -> Consumer Event m [a]
elements name f = element (name <> "s") $ listConsumer name f

sinkDimension :: MonadThrow m => Consumer Event m Dimension
sinkDimension = Dimension <$> getT "Name" <*> getT "Value"

sinkDimension' :: (MonadThrow m, Applicative m)
    => SimpleXML -> m Dimension
sinkDimension' xml = Dimension <$> xml .< "Name" <*> xml .< "Value"

fromDimension :: Dimension -> [QueryParam]
fromDimension Dimension{..} =
    [ "Name" |= dimensionName
    , "Value" |= dimensionValue
    ]
