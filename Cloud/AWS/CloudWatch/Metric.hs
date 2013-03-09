{-# LANGUAGE FlexibleContexts, RankNTypes, RecordWildCards #-}

module Cloud.AWS.CloudWatch.Metric
    ( listMetrics
    , getMetricStatistics
    , putMetricData
    ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Conduit
import Data.XML.Types (Event)
import Control.Applicative

import Cloud.AWS.CloudWatch.Internal
import Cloud.AWS.Lib.Query
import Cloud.AWS.Lib.Parser
import Cloud.AWS.CloudWatch.Types

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
        , "StartTime" |= start
        , "EndTime" |= end
        , "MetricName" |= mn
        , "Namespace" |= ns
        , "Period" |= pe
        , "Statistics" |.+ "member" |.#= sts
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
    , "Timestamp" |=? metricDatumTimestamp
    , "Unit" |=? metricDatumUnit
    ]

metricDatumValueParam :: MetricDatumValue -> QueryParam
metricDatumValueParam (MetricDatumValue v) = "Value" |= v
metricDatumValueParam (MetricDatumStatisticValues s) = "StatisticValues" |. fromStatisticSet s

fromStatisticSet :: StatisticSet -> [QueryParam]
fromStatisticSet StatisticSet{..} =
    [ "Maximum" |= statisticSetMaximum
    , "Minimum" |= statisticSetMinimum
    , "SampleCount" |= statisticSetSampleCount
    , "Sum" |= statisticSetSum
    ]
