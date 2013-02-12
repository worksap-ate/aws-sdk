module AWSTests.CloudWatchTests.MetricTests
    ( runMetricTests
    )
    where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (UTCTime(..), getCurrentTime)
import Test.Hspec

import AWS.CloudWatch
import AWS.CloudWatch.Types
import AWSTests.Util
import AWSTests.CloudWatchTests.Util

region :: Text
region = "ap-northeast-1"

runMetricTests :: IO ()
runMetricTests = do
    hspec listMetricsTest
    hspec putMetricDataTest

listMetricsTest :: Spec
listMetricsTest = do
    describe "Metric operations doesn't fail" $ do
        it "listMetrics doesn't throw any exception" $ do
            testCloudWatch region (listMetrics [] Nothing Nothing Nothing) `miss` anyHttpException

        it "getMetricStatistics doesn't throw any exception" $ do
            testCloudWatch region (do
                (metric:_, _) <- listMetrics [] Nothing Nothing Nothing
                end <- liftIO getCurrentTime
                let start = end { utctDayTime = utctDayTime end - 5*60 }
                getMetricStatistics [] start end (metricName metric) (metricNameSpace metric) 60 allStatistics Nothing
                ) `miss` anyHttpException

putMetricDataTest :: Spec
putMetricDataTest = do
    describe "putMetricData doesn't fail" $ do
        context "with Value" $ do
            it "putMetricData doesn't throw any exception" $ do
                testCloudWatch region (
                    putMetricData [dat {metricDatumValue = MetricDatumValue 42}] nameSpace
                    ) `miss` anyHttpException
        context "with StatisticSet" $ do
            it "putMetricData doesn't throw any exception" $ do
                testCloudWatch region (
                    putMetricData [dat {metricDatumValue = MetricDatumStatisticValues stat}] nameSpace
                    ) `miss` anyHttpException
  where
    dat = MetricDatum
        { metricDatumDimensions = [Dimension "dimTestName" "dimTestValue"]
        , metricDatumMetricName = "TestMetric"
        , metricDatumValue = error "Replace me!"
        , metricDatumTimestamp = Nothing
        , metricDatumUnit = Just "Bytes"
        }
    nameSpace = "TestNamespace"
    stat = StatisticSet
        { statisticSetMaximum = 100
        , statisticSetMinimum = 0
        , statisticSetSampleCount = 3.5
        , statisticSetSum = 50
        }
