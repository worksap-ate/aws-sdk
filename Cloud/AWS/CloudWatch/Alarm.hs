{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Cloud.AWS.CloudWatch.Alarm
    ( describeAlarms
    , describeAlarmsForMetric
    , putMetricAlarm
    , deleteAlarms
    , describeAlarmHistory
    , enableAlarmActions
    , disableAlarmActions
    , setAlarmState
    ) where

import Control.Applicative
import Data.Conduit
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.XML.Types (Event)

import Cloud.AWS.CloudWatch.Internal
import Cloud.AWS.CloudWatch.Types
import Cloud.AWS.Lib.Parser (getT, members, text)
import Cloud.AWS.Lib.Parser.Unordered hiding (getT)
import Cloud.AWS.Lib.Query

describeAlarms
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Text -- ^ The action name prefix.
    -> AlarmNameSpec -- ^ The alarm name prefix or a list of alarm names to retrieve information for.
    -> Maybe Int -- ^ The maximum number of alarm descriptions to retrieve.
    -> Maybe Text -- ^ The token returned by a previous call to indicate that there is more data available.
    -> Maybe StateValue -- ^ The state value to be used in matching alarms.
    -> CloudWatch m ([MetricAlarm], Maybe Text) -- ^ A list of information for the specified alarms and NextToken.
describeAlarms prefix spec maxRecords nextToken state =
    cloudWatchQuery "DescribeAlarms" params $
        (,) <$> members "MetricAlarms" sinkMetricAlarm <*> getT "NextToken"
  where
    params =
        [ "ActionPrefix" |=? prefix
        , specParam spec
        , "MaxRecords" |=? maxRecords
        , "NextToken" |=? nextToken
        , "StateValue" |=? state
        ]
    specParam AlarmSpecNothing = nothingParam
    specParam (AlarmSpecNamePrefix p) = "AlarmNamePrefix" |= p
    specParam (AlarmSpecNames ns) = "AlarmNames.member" |.#= ns

sinkMetricAlarm :: MonadThrow m => Consumer Event m MetricAlarm
sinkMetricAlarm =
    MetricAlarm
    <$> getT "AlarmDescription"
    <*> getT "StateUpdatedTimestamp"
    <*> members "InsufficientDataActions" text
    <*> getT "StateReasonData"
    <*> getT "AlarmArn"
    <*> getT "AlarmConfigurationUpdatedTimestamp"
    <*> getT "AlarmName"
    <*> getT "Period"
    <*> getT "StateValue"
    <*> members "OKActions" text
    <*> getT "ActionsEnabled"
    <*> getT "Namespace"
    <*> getT "Threshold"
    <*> getT "EvaluationPeriods"
    <*> getT "Namespace"
    <*> getT "Statistic"
    <*> members "AlarmActions" text
    <*> getT "Unit"
    <*> getT "StateReason"
    <*> members "Dimensions" sinkDimension
    <*> getT "ComparisonOperator"
    <*> getT "MetricName"

describeAlarmsForMetric
    :: (MonadBaseControl IO m, MonadResource m)
    => [Dimension] -- ^ The list of dimensions associated with the metric.
    -> Text -- ^ The name of the metric.
    -> Text -- ^ The namespace of the metric.
    -> Int -- ^ The period in seconds over which the statistic is applied.
    -> Statistic -- ^ The statistic for the metric.
    -> Maybe Text -- ^  The unit for the metric.
    -> CloudWatch m [MetricAlarm]
describeAlarmsForMetric dims name ns period stat unit =
    cloudWatchQuery "DescribeAlarmsForMetric" params $ members "MetricAlarms" sinkMetricAlarm
  where
    params =
        [ "Dimensions.member" |.#. map fromDimension dims
        , "MetricName" |= name
        , "Namespace" |= ns
        , "Period" |= period
        , "Statistic" |= stat
        , "Unit" |=? unit
        ]

putMetricAlarm
    :: forall m . (MonadBaseControl IO m, MonadResource m)
    => PutMetricAlarmRequest
    -> CloudWatch m ()
putMetricAlarm PutMetricAlarmRequest{..} =
    cloudWatchQuery "PutMetricAlarm" params
        $ xmlParser (.< "PutMetricAlarmResult")
  where
    params =
        [ "ActionsEnabled" |=? putMetricAlarmActionsEnabled
        , "AlarmActions.member" |.#= putMetricAlarmAlarmActions
        , "AlarmDescription" |=? putMetricAlarmAlarmDescription
        , "AlarmName" |= putMetricAlarmAlarmName
        , "ComparisonOperator" |= putMetricAlarmComparisonOperator
        , "Dimensions.member" |.#. map fromDimension putMetricAlarmDimensions
        , "EvaluationPeriods" |= putMetricAlarmEvaluationPeriods
        , "InsufficientDataActions.member" |.#= putMetricAlarmInsufficientDataActions
        , "MetricName" |= putMetricAlarmMetricName
        , "Namespace" |= putMetricAlarmNamespace
        , "OKActions.member" |.#= putMetricAlarmOKActions
        , "Period" |= putMetricAlarmPeriod
        , "Statistic" |= putMetricAlarmStatistic
        , "Threshold" |= putMetricAlarmThreshold
        , "Unit" |=? putMetricAlarmUnit
        ]

deleteAlarms
    :: (MonadBaseControl IO m, MonadResource m)
    => [Text] -- ^ A list of alarms to be deleted.
    -> CloudWatch m ()
deleteAlarms names =
    cloudWatchQuery "DeleteAlarms" ["AlarmNames.member" |.#= names]
        $ return ()

describeAlarmHistory
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Text -- ^ The name of the alarm.
    -> Maybe UTCTime -- ^ The ending date to retrieve alarm history.
    -> Maybe HistoryType -- ^ The type of alarm histories to retrieve.
    -> Maybe Int -- ^ The maximum number of alarm history records to retrieve.
    -> Maybe Text -- ^ The token returned by a previous call to indicate that there is more data available.
    -> Maybe UTCTime -- ^ The starting date to retrieve alarm history.
    -> CloudWatch m ([AlarmHistory], Maybe Text)
describeAlarmHistory alarm endDate type_ maxRecords nextToken startDate =
    cloudWatchQuery "DescribeAlarmHistory" params $
        (,) <$> members "AlarmHistoryItems" sinkAlarmHistory <*> getT "NextToken"
  where
    params =
        [ "AlarmName" |=? alarm
        , "EndDate" |=? endDate
        , "HistoryItemType" |=? type_
        , "MaxRecords" |=? maxRecords
        , "NextToken" |=? nextToken
        , "StartDate" |=? startDate
        ]

sinkAlarmHistory :: MonadThrow m => Consumer Event m AlarmHistory
sinkAlarmHistory =
    AlarmHistory
    <$> getT "Timestamp"
    <*> getT "HistoryItemType"
    <*> getT "AlarmName"
    <*> getT "HistoryData"
    <*> getT "HistorySummary"

enableAlarmActions
    :: (MonadBaseControl IO m, MonadResource m)
    => [Text] -- ^ The names of the alarms to enable actions for.
    -> CloudWatch m ()
enableAlarmActions alarms =
    cloudWatchQuery "EnableAlarmActions" ["AlarmNames.member" |.#= alarms]
        $ return ()

disableAlarmActions
    :: (MonadBaseControl IO m, MonadResource m)
    => [Text] -- ^ The names of the alarms to enable actions for.
    -> CloudWatch m ()
disableAlarmActions alarms =
    cloudWatchQuery "DisableAlarmActions" ["AlarmNames.member" |.#= alarms]
        $ return ()

setAlarmState
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The name for the alarm.
    -> Text -- ^ The reason that this alarm is set to this specific state (in human-readable text format)
    -> Text -- ^ The reason that this alarm is set to this specific state (in machine-readable JSON format)
    -> StateValue -- ^ The value of the state.
    -> CloudWatch m ()
setAlarmState alarm reason dat state =
    cloudWatchQuery "SetAlarmState" params $ return ()
  where
    params =
        [ "AlarmName" |= alarm
        , "StateReason" |= reason
        , "StateReasonData" |= dat
        , "StateValue" |= state
        ]
