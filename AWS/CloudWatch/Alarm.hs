{-# LANGUAGE FlexibleContexts #-}
module AWS.CloudWatch.Alarm
    ( describeAlarms
    ) where

import Control.Applicative
import Data.Conduit
import Data.Text (Text)
import Data.XML.Types (Event)

import AWS.CloudWatch.Internal
import AWS.CloudWatch.Types
import AWS.Lib.Parser (getT, members, text)
import AWS.Lib.Query
import AWS.Util (toText)

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
        , "MaxRecords" |=? toText <$> maxRecords
        , "NextToken" |=? nextToken
        , "StateValue" |=? s <$> state
        ]
    specParam AlarmSpecNothing = nothingParam
    specParam (AlarmSpecNamePrefix p) = "AlarmNamePrefix" |= p
    specParam (AlarmSpecNames ns) = "AlarmNames.member" |.#= ns

    s :: StateValue -> Text
    s StateValueOk = "OK"
    s StateValueAlarm = "ALARM"
    s StateValueInsufficientData = "INSUFFICIENT_DATA"

sinkMetricAlarm :: MonadThrow m => GLSink Event m MetricAlarm
sinkMetricAlarm =
    MetricAlarm
    <$> getT "AlarmDescription"
    <*> getT "StateUpdatedTimestamp"
    <*> members "InsufficientDataActions" text
    <*> getT "StateReasonData"
    <*> getT "AlarmArn"
    <*> getT "AlarmConfigurationUpdatedTimestamp"
    <*> getT "AlarmName"
    <*> getT "StateValue"
    <*> getT "Period"
    <*> members "OKActions" text
    <*> getT "ActionsEnabled"
    <*> getT "Namespace"
    <*> getT "Threshold"
    <*> getT "EvaluationPeriods"
    <*> getT "Statistic"
    <*> members "AlarmActions" text
    <*> getT "StateReason"
    <*> members "Dimensions" sinkDimension
    <*> getT "ComparisonOperator"
    <*> getT "MetricName"
