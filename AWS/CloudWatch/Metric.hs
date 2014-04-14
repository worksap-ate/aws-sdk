{-# LANGUAGE FlexibleContexts, RankNTypes, RecordWildCards, CPP #-}

module AWS.CloudWatch.Metric
    ( listMetrics
    , getMetricStatistics
    , putMetricData
    ) where

#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)
#endif
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Conduit
import Data.XML.Types (Event)
import Control.Applicative

import AWS.Util
import AWS.CloudWatch.Internal
import AWS.Lib.Query
import AWS.Lib.Parser
import AWS.CloudWatch.Types

dimensionFiltersParam :: [DimensionFilter] -> QueryParam
dimensionFiltersParam =
    ("Dimensions" |.+) . ("member" |.#.) . map filterParams
  where
    filterParams (k, v) =
        [ "Name" |= k
        , "Value" |= v
        ]

listMetrics
    :: (MonadBaseControl IO m, MonadResource m)
    => [DimensionFilter] -- ^ Dimensions
    -> Maybe Text -- ^ MetricName
    -> Maybe Text -- ^ Namespace
    -> Maybe Text -- ^ NextToken
    -> CloudWatch m ([Metric], Maybe Text)
listMetrics ds mn ns nt = cloudWatchQuery "ListMetrics" params $
    (,) <$> members "Metrics" sinkMetric <*> getT "NextToken"
  where
    params =
        [ dimensionFiltersParam ds
        , "MetricName" |=? mn
        , "Namespace" |=? ns
        , "NextToken" |=? nt
        ]

sinkMetric :: MonadThrow m => Consumer Event m Metric
sinkMetric =
    Metric
    <$> members "Dimensions" sinkDimension
    <*> getT "MetricName"
    <*> getT "Namespace"

getMetricStatistics
    :: (MonadBaseControl IO m, MonadResource m)
    => [DimensionFilter]
    -> UTCTime -- ^ StartTime
    -> UTCTime -- ^ EndTime
    -> Text -- ^ MetricName
    -> Text -- ^ Namespace
    -> Int -- ^ Period
    -> [Statistic] -- ^ Statistics
    -> Maybe Text -- ^ Unit
    -> CloudWatch m ([Datapoint], Text) -- ^ Datapoints and Label
getMetricStatistics ds start end mn ns pe sts unit =
    cloudWatchQuery "GetMetricStatistics" params $ (,)
        <$> members "Datapoints" (Datapoint
            <$> getT "Timestamp"
            <*> getT "SampleCount"
            <*> getT "Unit"
            <*> getT "Minimum"
            <*> getT "Maximum"
            <*> getT "Sum"
            <*> getT "Average"
            )
        <*> getT "Label"
  where
    params =
        [ dimensionFiltersParam ds
        , "StartTime" |= timeToText start
        , "EndTime" |= timeToText end
        , "MetricName" |= mn
        , "Namespace" |= ns
        , "Period" |= toText pe
        , "Statistics" |.+ "member" |.#= map stringifyStatistic sts
        , "Unit" |=? unit
        ]

putMetricData
    :: (MonadBaseControl IO m, MonadResource m)
    => [MetricDatum] -- ^ A list of data describing the metric.
    -> Text -- ^ The namespace for the metric data.
    -> CloudWatch m ()
putMetricData dats ns =
    cloudWatchQuery "PutMetricData" params $ return ()
  where
    params =
        [ "MetricData.member" |.#. map fromMetricDatum dats
        , "Namespace" |= ns
        ]

fromMetricDatum :: MetricDatum -> [QueryParam]
fromMetricDatum MetricDatum{..} =
    [ "Dimensions.member" |.#. map fromDimension metricDatumDimensions
    , "MetricName" |= metricDatumMetricName
    , metricDatumValueParam metricDatumValue
    , "Timestamp" |=? timeToText <$> metricDatumTimestamp
    , "Unit" |=? metricDatumUnit
    ]

metricDatumValueParam :: MetricDatumValue -> QueryParam
metricDatumValueParam (MetricDatumValue v) = "Value" |= toText v
metricDatumValueParam (MetricDatumStatisticValues s) = "StatisticValues" |. fromStatisticSet s

fromStatisticSet :: StatisticSet -> [QueryParam]
fromStatisticSet StatisticSet{..} =
    [ "Maximum" |= toText statisticSetMaximum
    , "Minimum" |= toText statisticSetMinimum
    , "SampleCount" |= toText statisticSetSampleCount
    , "Sum" |= toText statisticSetSum
    ]
