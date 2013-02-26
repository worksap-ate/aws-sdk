{-# LANGUAGE FlexibleContexts #-}

module AWS.RDS.EventSubscription
    ( describeEventSubscriptions
    , createEventSubscription
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Conduit
import Data.Text (Text)
import Data.XML.Types (Event)

import AWS.Lib.Parser
import AWS.Lib.Query
import AWS.RDS.Internal
import AWS.RDS.Types (EventSubscription(..), SourceType)
import AWS.Util (toText, boolToText)

describeEventSubscriptions
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Text -- ^ SubscriptionName
    -> Maybe Text -- ^ Marker
    -> Maybe Int -- ^ MaxRecords
    -> RDS m [EventSubscription]
describeEventSubscriptions name marker maxRecords =
    rdsQuery "DescribeEventSubscriptions" params $
        elements' "EventSubscriptionsList" "EventSubscription" eventSubscriptionSink
  where
    params =
        [ "SubscriptionName" |=? name
        , "Marker" |=? marker
        , "MaxRecords" |=? toText <$> maxRecords
        ]

eventSubscriptionSink
    :: MonadThrow m
    => Consumer Event m EventSubscription
eventSubscriptionSink = EventSubscription
    <$> getT "Enabled"
    <*> getT "CustomerAwsId"
    <*> getT "SourceType"
    <*> getT "Status"
    <*> elements' "SourceIdsList" "SourceId" text
    <*> getT "SubscriptionCreationTime"
    <*> elements' "EventCategoriesList" "EventCategory" text
    <*> getT "CustSubscriptionId"
    <*> getT "SnsTopicArn"

createEventSubscription
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Bool -- ^ Enabled
    -> [Text] -- ^ EventCategories
    -> Text -- ^ SnsTopicArn
    -> [Text] -- ^ SourceIds
    -> Maybe SourceType -- ^ SourceType
    -> Text -- ^ SubscriptionName
    -> RDS m EventSubscription
createEventSubscription enabled ecs topic sids stype name =
    rdsQuery "CreateEventSubscription" params $
        element "EventSubscription" eventSubscriptionSink
  where
    params =
        [ "Enabled" |=? boolToText <$> enabled
        , "EventCategories.member" |.#= ecs
        , "SnsTopicArn" |= topic
        , "SourceIds.member" |.#= sids
        , "SourceType" |=? sourceTypeToText <$> stype
        , "SubscriptionName" |= name
        ]
