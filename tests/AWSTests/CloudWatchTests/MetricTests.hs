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
