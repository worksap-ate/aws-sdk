{-# LANGUAGE TemplateHaskell #-}
module AWS.CloudWatch.Types
    where

import AWS.Lib.FromText

data Metric = Metric
    { metricDimensions :: [Dimension]
    , metricName :: Text
    , metricNameSpace :: Text
    }
  deriving (Show, Eq)

data Dimension = Dimension
    { dimensionName :: Text
    , dimensionValue :: Text
    }
  deriving (Show, Eq)

type DimensionFilter = (Text, Text)

data Datapoint = Datapoint
    { datapointTimestamp :: UTCTime
    , datapointSampleCount :: Maybe Double
    , datapointUnit :: Text
    , datapointMinimum :: Maybe Double
    , datapointMaximum :: Maybe Double
    , datapointSum :: Maybe Double
    , datapointAverage :: Maybe Double
    }
  deriving (Show, Eq)

data Statistic
    = StatisticAverage
    | StatisticSum
    | StatisticSampleCount
    | StatisticMaximum
    | StatisticMinimum
  deriving (Show, Eq, Read)
deriveFromText "Statistic" ["Average", "Sum", "SampleCount", "Maximum", "Minimum"]

allStatistics :: [Statistic]
allStatistics =
    [ StatisticAverage
    , StatisticSum
    , StatisticSampleCount
    , StatisticMaximum
    , StatisticMinimum
    ]

data AlarmNameSpec
    = AlarmSpecNothing
    | AlarmSpecNamePrefix Text
    | AlarmSpecNames [Text]
  deriving (Show, Eq)

data StateValue
    = StateValueOk
    | StateValueAlarm
    | StateValueInsufficientData
  deriving (Show, Eq, Read)
deriveFromText "StateValue" ["OK", "ALARM", "INSUFFICIENT_DATA"]

data ComparisonOperator
    = GreaterThanOrEqualToThreshold
    | GreaterThanThreshold
    | LessThanThreshold
    | LessThanOrEqualToThreshold
  deriving (Show, Eq, Read)
deriveFromText "ComparisonOperator"
    [ "GreaterThanOrEqualToThreshold"
    , "GreaterThanThreshold"
    , "LessThanThreshold"
    , "LessThanOrEqualToThreshold"
    ]

data MetricAlarm = MetricAlarm
    { metricAlarmAlarmDescription :: Maybe Text
    , metricAlarmStateUpdatedTimestamp :: UTCTime
    , metricAlarmInsufficientDataActions :: [Text]
    , metricAlarmStateReasonData :: Maybe Text
    , metricAlarmAlarmArn :: Text
    , metricAlarmConfigurationUpdatedTimestamp :: UTCTime
    , metricAlarmAlarmName :: Text
    , metricAlarmStateValue :: StateValue
    , metricAlarmPeriod :: Int
    , metricAlarmOKActions :: [Text]
    , metricAlarmActionsEnabled :: Bool
    , metricAlarmNamespace :: Text
    , metricAlarmThreshold :: Double
    , metricAlarmEvaluationPeriods :: Int
    , metricAlarmStatistic :: Statistic
    , metricAlarmAlarmActions :: [Text]
    , metricAlarmUnit :: Maybe Text
    , metricAlarmStateReason :: Maybe Text
    , metricAlarmDimensions :: [Dimension]
    , metricAlarmComparisonOperator :: ComparisonOperator
    , metricAlarmMetricName :: Text
    }
  deriving (Show, Eq)
