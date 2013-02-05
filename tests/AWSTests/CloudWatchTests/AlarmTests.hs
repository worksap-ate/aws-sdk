{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module AWSTests.CloudWatchTests.AlarmTests
    ( runAlarmTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import AWS.CloudWatch
import AWS.CloudWatch.Types
import AWSTests.Util
import AWSTests.CloudWatchTests.Util

region :: Text
region = "ap-northeast-1"

runAlarmTests :: IO ()
runAlarmTests = do
    hspec describeAlarmsTest

describeAlarmsTest :: Spec
describeAlarmsTest = do
    describe "Alarm operations doesn't fail" $ do
        it "describeAlarms doesn't throw any exception" $ do
            testCloudWatch region (describeAlarms Nothing AlarmSpecNothing Nothing Nothing Nothing) `miss` anyHttpException

            -- Expect NextToken is returned
            testCloudWatch region (describeAlarms Nothing AlarmSpecNothing (Just 1) Nothing Nothing) `miss` anyHttpException

        it "describeAlarmsForMetric doesn't throw any exception" $ do
            testCloudWatch region (do
                (MetricAlarm{..}:_, _) <- describeAlarms Nothing AlarmSpecNothing Nothing Nothing Nothing
                describeAlarmsForMetric metricAlarmDimensions metricAlarmMetricName metricAlarmNamespace metricAlarmPeriod metricAlarmStatistic Nothing
                ) `miss` anyHttpException
