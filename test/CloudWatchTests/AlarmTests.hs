{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module CloudWatchTests.AlarmTests
    ( runAlarmTests
    )
    where

import Data.Text (Text)
import Test.Hspec

import Cloud.AWS.CloudWatch
import Cloud.AWS.CloudWatch.Types
import Util
import CloudWatchTests.Util

region :: Text
region = "ap-northeast-1"

runAlarmTests :: IO ()
runAlarmTests = do
    hspec describeAlarmsTest
    hspec putMetricAlarmTest
    hspec describeAlarmHistoryTest
    hspec alarmActionsTest
    hspec setAlarmStateTest

describeAlarmsTest :: Spec
describeAlarmsTest = do
    describe "Alarm operations doesn't fail" $ do
        it "describeAlarms doesn't throw any exception" $ do
            testCloudWatch region (describeAlarms Nothing AlarmSpecNothing Nothing Nothing Nothing) `miss` anyConnectionException

            -- Expect NextToken is returned
            testCloudWatch region (describeAlarms Nothing AlarmSpecNothing (Just 1) Nothing Nothing) `miss` anyConnectionException

        it "describeAlarmsForMetric doesn't throw any exception" $ do
            testCloudWatch region (do
                (MetricAlarm{..}:_, _) <- describeAlarms Nothing AlarmSpecNothing Nothing Nothing Nothing
                describeAlarmsForMetric metricAlarmDimensions metricAlarmMetricName metricAlarmNamespace metricAlarmPeriod metricAlarmStatistic Nothing
                ) `miss` anyConnectionException

putMetricAlarmTest :: Spec
putMetricAlarmTest =
    describe "putMetricAlarm/deleteAlarms doesn't fail" $ do
        it "putMetricAlarm/deleteAlarms doesn't throw any exception" $ do
            testCloudWatch region (do
                (metric:_, _) <- listMetrics [] Nothing Nothing Nothing
                let req = PutMetricAlarmRequest
                            { putMetricAlarmActionsEnabled = Just False
                            , putMetricAlarmAlarmActions = []
                            , putMetricAlarmAlarmDescription = Just "putMetricAlarmTest Description"
                            , putMetricAlarmAlarmName = "putMetricAlarmTest"
                            , putMetricAlarmComparisonOperator = GreaterThanThreshold
                            , putMetricAlarmDimensions = []
                            , putMetricAlarmEvaluationPeriods = 5
                            , putMetricAlarmInsufficientDataActions = []
                            , putMetricAlarmMetricName = metricName metric
                            , putMetricAlarmNamespace = metricNameSpace metric
                            , putMetricAlarmOKActions = []
                            , putMetricAlarmPeriod = 5 * 60
                            , putMetricAlarmStatistic = StatisticSampleCount
                            , putMetricAlarmThreshold = 42.0
                            , putMetricAlarmUnit = Just "Count"
                            }
                putMetricAlarm req
                deleteAlarms [putMetricAlarmAlarmName req]
                ) `miss` anyConnectionException

describeAlarmHistoryTest :: Spec
describeAlarmHistoryTest =
    describe "describeAlarmHistory doesn't fail" $ do
        it "describeAlarmHistory doesn't throw any exception" $ do
            testCloudWatch region (do
                describeAlarmHistory Nothing Nothing Nothing Nothing Nothing Nothing
                -- Expect NextToken is returned
                describeAlarmHistory Nothing Nothing Nothing (Just 1) Nothing Nothing
                ) `miss` anyConnectionException

alarmActionsTest :: Spec
alarmActionsTest =
    describe "{disable,enable}AlarmActions doesn't fail" $ do
        it "{disable,enable}AlarmActions doesn't throw any exception" $ do
            testCloudWatch region (do
                (alarm:_, _) <- describeAlarms Nothing AlarmSpecNothing Nothing Nothing Nothing
                let name = metricAlarmAlarmName alarm
                disableAlarmActions [name]
                enableAlarmActions [name]
                ) `miss` anyConnectionException

setAlarmStateTest :: Spec
setAlarmStateTest =
    describe "setAlarmState doesn't fail" $ do
        it "setAlarmState doesn't throw any exception" $ do
            testCloudWatch region (do
                (alarm:_, _) <- describeAlarms Nothing AlarmSpecNothing Nothing Nothing Nothing
                let name = metricAlarmAlarmName alarm
                setAlarmState name "test" "[]" StateValueInsufficientData
                ) `miss` anyConnectionException
