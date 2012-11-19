module AWS.CloudWatch.Types
    where

import Data.Text (Text)
import Data.Time (UTCTime)

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
  deriving (Show, Eq)

allStatistics :: [Statistic]
allStatistics =
    [ StatisticAverage
    , StatisticSum
    , StatisticSampleCount
    , StatisticMaximum
    , StatisticMinimum
    ]
