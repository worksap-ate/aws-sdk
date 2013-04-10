{-# LANGUAGE TemplateHaskell #-}
module Cloud.AWS.CloudWatch.Types
    where

import Control.Applicative
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Cloud.AWS.Lib.FromText
import Cloud.AWS.Lib.ToText

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

instance ToText Statistic where
    toText StatisticAverage     = "Average"
    toText StatisticSum         = "Sum"
    toText StatisticSampleCount = "SampleCount"
    toText StatisticMaximum     = "Maximum"
    toText StatisticMinimum     = "Minimum"

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

instance ToText StateValue where
    toText StateValueOk = "OK"
    toText StateValueAlarm = "ALARM"
    toText StateValueInsufficientData = "INSUFFICIENT_DATA"

deriveFromText "StateValue" ["OK", "ALARM", "INSUFFICIENT_DATA"]

data ComparisonOperator
    = GreaterThanOrEqualToThreshold
    | GreaterThanThreshold
    | LessThanThreshold
    | LessThanOrEqualToThreshold
  deriving (Show, Eq, Read)

instance ToText ComparisonOperator where
    toText = T.pack . show

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
    , metricAlarmPeriod :: Int
    , metricAlarmStateValue :: StateValue
    , metricAlarmOKActions :: [Text]
    , metricAlarmActionsEnabled :: Bool
    , metricAlarmNamespace1 :: Maybe Text
    , metricAlarmThreshold :: Double
    , metricAlarmEvaluationPeriods :: Int
    , metricAlarmNamespace2 :: Maybe Text
    , metricAlarmStatistic :: Statistic
    , metricAlarmAlarmActions :: [Text]
    , metricAlarmUnit :: Maybe Text
    , metricAlarmStateReason :: Maybe Text
    , metricAlarmDimensions :: [Dimension]
    , metricAlarmComparisonOperator :: ComparisonOperator
    , metricAlarmMetricName :: Text
    }
  deriving (Show, Eq)

metricAlarmNamespace :: MetricAlarm -> Text
metricAlarmNamespace ma = fromJust
    $ metricAlarmNamespace1 ma <|> metricAlarmNamespace2 ma

data PutMetricAlarmRequest = PutMetricAlarmRequest
    { putMetricAlarmActionsEnabled :: Maybe Bool
    , putMetricAlarmAlarmActions :: [Text]
    , putMetricAlarmAlarmDescription :: Maybe Text
    , putMetricAlarmAlarmName :: Text
    , putMetricAlarmComparisonOperator :: ComparisonOperator
    , putMetricAlarmDimensions :: [Dimension]
    , putMetricAlarmEvaluationPeriods :: Int
    , putMetricAlarmInsufficientDataActions :: [Text]
    , putMetricAlarmMetricName :: Text
    , putMetricAlarmNamespace :: Text
    , putMetricAlarmOKActions :: [Text]
    , putMetricAlarmPeriod :: Int
    , putMetricAlarmStatistic :: Statistic
    , putMetricAlarmThreshold :: Double
    , putMetricAlarmUnit :: Maybe Text
    }
  deriving (Show, Eq)

data AlarmHistory = AlarmHistory
    { alarmHistoryTimestamp :: UTCTime
    , alarmHistoryHistoryItemType :: HistoryType
    , alarmHistoryAlarmName :: Text
    , alarmHistoryHistoryData :: Text
    , alarmHistoryHistorySummary :: Text
    }
  deriving (Show, Eq)

data HistoryType
    = HistoryTypeConfigurationUpdate
    | HistoryTypeStateUpdate
    | HistoryTypeAction
  deriving (Show, Eq, Read)

instance ToText HistoryType where
    toText HistoryTypeConfigurationUpdate = "ConfigurationUpdate"
    toText HistoryTypeStateUpdate = "StateUpdate"
    toText HistoryTypeAction = "Action"

deriveFromText "HistoryType" ["ConfigurationUpdate", "StateUpdate", "Action"]

data MetricDatum = MetricDatum
    { metricDatumDimensions :: [Dimension]
    , metricDatumMetricName :: Text
    , metricDatumTimestamp :: Maybe UTCTime
    , metricDatumUnit :: Maybe Text
    , metricDatumValue :: MetricDatumValue
    }
  deriving (Show, Eq)

data MetricDatumValue
    = MetricDatumValue Double
    | MetricDatumStatisticValues StatisticSet
  deriving (Show, Eq)

data StatisticSet = StatisticSet
    { statisticSetMaximum :: Double
    , statisticSetMinimum :: Double
    , statisticSetSampleCount :: Double
    , statisticSetSum :: Double
    }
  deriving (Show, Eq)
