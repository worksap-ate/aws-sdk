{-# LANGUAGE FlexibleContexts, RankNTypes, RecordWildCards, CPP #-}

module AWS.CloudWatch.Internal
    where

import Control.Applicative
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)
#endif
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Conduit
import Data.Monoid ((<>))
import Data.XML.Types (Event(..))

import AWS.Class
import AWS.Lib.Query
import AWS.Lib.Parser
import AWS.CloudWatch.Types

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

fromDimension :: Dimension -> [QueryParam]
fromDimension Dimension{..} =
    [ "Name" |= dimensionName
    , "Value" |= dimensionValue
    ]

stringifyStatistic :: Statistic -> Text
stringifyStatistic StatisticAverage     = "Average"
stringifyStatistic StatisticSum         = "Sum"
stringifyStatistic StatisticSampleCount = "SampleCount"
stringifyStatistic StatisticMaximum     = "Maximum"
stringifyStatistic StatisticMinimum     = "Minimum"
