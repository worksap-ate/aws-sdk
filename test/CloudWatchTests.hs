module Main where

import CloudWatchTests.AlarmTests
import CloudWatchTests.MetricTests

main :: IO ()
main = do
    runAlarmTests
    runMetricTests
