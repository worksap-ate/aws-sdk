{-# LANGUAGE FlexibleContexts, RankNTypes, RecordWildCards #-}

module Cloud.AWS.CloudWatch.Metric
    ( listMetrics
    , getMetricStatistics
    , putMetricData
    ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Control.Applicative
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)
import Cloud.AWS.Lib.Parser.Unordered (XmlElement, (.<))

import Cloud.AWS.CloudWatch.Internal
import Cloud.AWS.Lib.Query
import Cloud.AWS.Lib.Parser (members, nodata)
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
listMetrics ds mn ns nt = cloudWatchQuery "ListMetrics" params $ \xml ->
    (,) <$> members "Metrics" sinkMetric xml <*> xml .< "NextToken"
  where
    params =
        [ dimensionFiltersParam ds
        , "MetricName" |=? mn
        , "Namespace" |=? ns
        , "NextToken" |=? nt
        ]

sinkMetric :: (MonadThrow m, Applicative m)
    => XmlElement -> m Metric
sinkMetric xml =
    Metric
    <$> members "Dimensions" sinkDimension xml
    <*> xml .< "MetricName"
    <*> xml .< "Namespace"

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
    cloudWatchQuery "GetMetricStatistics" params $ \xml -> (,)
        <$> members "Datapoints" (\xml' -> Datapoint
            <$> xml' .< "Timestamp"
            <*> xml' .< "SampleCount"
            <*> xml' .< "Unit"
            <*> xml' .< "Minimum"
            <*> xml' .< "Maximum"
            <*> xml' .< "Sum"
            <*> xml' .< "Average"
            ) xml
        <*> xml .< "Label"
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
    cloudWatchQuery "PutMetricData" params nodata
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
