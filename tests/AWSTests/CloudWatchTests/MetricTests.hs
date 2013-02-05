module AWSTests.CloudWatchTests.MetricTests
    ( runMetricTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.CloudWatch
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
