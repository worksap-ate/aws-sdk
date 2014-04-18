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

import Cloud.AWS.Lib.Parser.Unordered (XmlElement, (.<), content)
import Control.Applicative
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)
import Data.Text (Text)
import Data.Time (UTCTime)

import Cloud.AWS.CloudWatch.Internal
import Cloud.AWS.CloudWatch.Types
import Cloud.AWS.Lib.Parser (members, nodata)
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
    cloudWatchQuery "DescribeAlarms" params $ \xml ->
        (,) <$> members "MetricAlarms" sinkMetricAlarm xml <*> xml .< "NextToken"
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

sinkMetricAlarm :: (MonadThrow m, Applicative m)
    => XmlElement -> m MetricAlarm
sinkMetricAlarm xml =
    MetricAlarm
    <$> xml .< "AlarmDescription"
    <*> xml .< "StateUpdatedTimestamp"
    <*> members "InsufficientDataActions" content xml
    <*> xml .< "StateReasonData"
    <*> xml .< "AlarmArn"
    <*> xml .< "AlarmConfigurationUpdatedTimestamp"
    <*> xml .< "AlarmName"
    <*> xml .< "Period"
    <*> xml .< "StateValue"
    <*> members "OKActions" content xml
    <*> xml .< "ActionsEnabled"
    <*> xml .< "Namespace"
    <*> xml .< "Threshold"
    <*> xml .< "EvaluationPeriods"
    <*> xml .< "Statistic"
    <*> members "AlarmActions" content xml
    <*> xml .< "Unit"
    <*> xml .< "StateReason"
    <*> members "Dimensions" sinkDimension xml
    <*> xml .< "ComparisonOperator"
    <*> xml .< "MetricName"

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
    cloudWatchQuery "DescribeAlarmsForMetric" params $ \xml ->
        members "MetricAlarms" sinkMetricAlarm xml
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
    cloudWatchQuery "PutMetricAlarm" params nodata
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
    cloudWatchQuery "DeleteAlarms" ["AlarmNames.member" |.#= names] nodata

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
    cloudWatchQuery "DescribeAlarmHistory" params $ \xml ->
        (,) <$> members "AlarmHistoryItems" sinkAlarmHistory xml <*> xml .< "NextToken"
  where
    params =
        [ "AlarmName" |=? alarm
        , "EndDate" |=? endDate
        , "HistoryItemType" |=? type_
        , "MaxRecords" |=? maxRecords
        , "NextToken" |=? nextToken
        , "StartDate" |=? startDate
        ]

sinkAlarmHistory :: (MonadThrow m, Applicative m)
    => XmlElement -> m AlarmHistory
sinkAlarmHistory xml =
    AlarmHistory
    <$> xml .< "Timestamp"
    <*> xml .< "HistoryItemType"
    <*> xml .< "AlarmName"
    <*> xml .< "HistoryData"
    <*> xml .< "HistorySummary"

enableAlarmActions
    :: (MonadBaseControl IO m, MonadResource m)
    => [Text] -- ^ The names of the alarms to enable actions for.
    -> CloudWatch m ()
enableAlarmActions alarms =
    cloudWatchQuery "EnableAlarmActions" ["AlarmNames.member" |.#= alarms] nodata

disableAlarmActions
    :: (MonadBaseControl IO m, MonadResource m)
    => [Text] -- ^ The names of the alarms to enable actions for.
    -> CloudWatch m ()
disableAlarmActions alarms =
    cloudWatchQuery "DisableAlarmActions" ["AlarmNames.member" |.#= alarms] nodata

setAlarmState
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The name for the alarm.
    -> Text -- ^ The reason that this alarm is set to this specific state (in human-readable text format)
    -> Text -- ^ The reason that this alarm is set to this specific state (in machine-readable JSON format)
    -> StateValue -- ^ The value of the state.
    -> CloudWatch m ()
setAlarmState alarm reason dat state =
    cloudWatchQuery "SetAlarmState" params nodata
  where
    params =
        [ "AlarmName" |= alarm
        , "StateReason" |= reason
        , "StateReasonData" |= dat
        , "StateValue" |= state
        ]
