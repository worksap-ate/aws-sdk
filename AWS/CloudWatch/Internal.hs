{-# LANGUAGE FlexibleContexts, RankNTypes, RecordWildCards #-}

module AWS.CloudWatch.Internal
    where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Conduit
import Data.Monoid ((<>))
import Data.XML.Types (Event(..))

import AWS.Class
import AWS.Lib.Query
import AWS.Lib.Parser
import AWS.CloudWatch.Types

apiVersion :: ByteString
apiVersion = "2010-08-01"

type CloudWatch m a = AWS AWSContext m a

cloudWatchQuery
    :: (MonadBaseControl IO m, MonadResource m)
    => ByteString -- ^ Action
    -> [QueryParam]
    -> GLSink Event m a
    -> CloudWatch m a
cloudWatchQuery = commonQuery apiVersion

elements :: MonadThrow m
    => Text
    -> GLSink Event m a
    -> GLSink Event m [a]
elements name f = element (name <> "s") $ listConsumer name f

sinkDimension :: MonadThrow m => GLSink Event m Dimension
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
