{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.CloudWatch.Metric
    ( listMetrics
    , getMetricStatistics
    ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Applicative

import AWS.Util
import AWS.CloudWatch.Internal
import AWS.Lib.Query
import AWS.Lib.Parser
import AWS.CloudWatch.Types

dimensionFilterParams :: [DimensionFilter] -> QueryParam
dimensionFilterParams filters =
    StructArrayParams "Dimensions.member" $ map f filters
  where
    f (k, v) = [("Name", k), ("Value", v)]

listMetrics
    :: (MonadBaseControl IO m, MonadResource m)
    => [DimensionFilter] -- ^ Dimensions
    -> Maybe Text -- ^ MetricName
    -> Maybe Text -- ^ Namespace
    -> Maybe Text -- ^ NextToken
    -> CloudWatch m [Metric]
listMetrics ds mn ns nt = cloudWatchQuery "ListMetrics" params $
    members "Metrics" $ Metric
        <$> members "Dimensions" (Dimension
            <$> getT "Name"
            <*> getT "Value"
            )
        <*> getT "MetricName"
        <*> getT "Namespace"
  where
    params =
        dimensionFilterParams ds :
        maybeParams
        [ ("MetricName", mn)
        , ("Namespace", ns)
        , ("NextToken", nt)
        ]

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
            <$> getF "Timestamp" textToTime
            <*> getMT "SampleCount"
            <*> getT "Unit"
            <*> getMT "Minimum"
            <*> getMT "Maximum"
            <*> getMT "Sum"
            <*> getMT "Average"
            )
        <*> getT "Label"
  where
    params = dimensionFilterParams ds :
        [ ValueParam "StartTime" $ timeToText start
        , ValueParam "EndTime" $ timeToText end
        , ValueParam "MetricName" mn
        , ValueParam "Namespace" ns
        , ValueParam "Period" $ toText pe
        , ArrayParams "Statistics.member" $ map s sts
        ] ++ maybeParams [("Unit", unit)]
    s StatisticAverage     = "Average"
    s StatisticSum         = "Sum"
    s StatisticSampleCount = "SampleCount"
    s StatisticMaximum     = "Maximum"
    s StatisticMinimum     = "Minimum"
