{-# LANGUAGE FlexibleContexts #-}

module Cloud.AWS.RDS.EventSubscription
    ( describeEventSubscriptions
    , createEventSubscription
    , deleteEventSubscription
    , modifyEventSubscription
    , addSourceIdentifierToSubscription
    ) where

import Cloud.AWS.Lib.Parser.Unordered (XmlElement, (.<), content, element)
import Control.Applicative
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)
import Data.Text (Text)

import Cloud.AWS.Lib.Query
import Cloud.AWS.RDS.Internal
import Cloud.AWS.RDS.Types (EventSubscription(..), SourceType)

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
        , "MaxRecords" |=? maxRecords
        ]

eventSubscriptionSink
    :: (MonadThrow m, Applicative m)
    => XmlElement -> m EventSubscription
eventSubscriptionSink xml = EventSubscription
    <$> xml .< "Enabled"
    <*> xml .< "CustomerAwsId"
    <*> xml .< "SourceType"
    <*> xml .< "Status"
    <*> elements' "SourceIdsList" "SourceId" content xml
    <*> xml .< "SubscriptionCreationTime"
    <*> elements' "EventCategoriesList" "EventCategory" content xml
    <*> xml .< "CustSubscriptionId"
    <*> xml .< "SnsTopicArn"

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
        [ "Enabled" |=? enabled
        , "EventCategories.member" |.#= ecs
        , "SnsTopicArn" |= topic
        , "SourceIds.member" |.#= sids
        , "SourceType" |=? stype
        , "SubscriptionName" |= name
        ]

deleteEventSubscription
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ SubscriptionName
    -> RDS m EventSubscription
deleteEventSubscription name =
    rdsQuery "DeleteEventSubscription" ["SubscriptionName" |= name] $
        element "EventSubscription" eventSubscriptionSink

modifyEventSubscription
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Bool -- ^ Enabled
    -> [Text] -- ^ EventCategories
    -> Maybe Text -- ^ SnsTopicArn
    -> Maybe SourceType -- ^ SourceType
    -> Text -- ^ SubscriptionName
    -> RDS m EventSubscription
modifyEventSubscription enabled ecs topic stype name =
    rdsQuery "ModifyEventSubscription" params $
        element "EventSubscription" eventSubscriptionSink
  where
    params =
        [ "Enabled" |=? enabled
        , "EventCategories.member" |.#= ecs
        , "SnsTopicArn" |=? topic
        , "SourceType" |=? stype
        , "SubscriptionName" |= name
        ]

addSourceIdentifierToSubscription
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ SourceIdentifier
    -> Text -- ^ SubscriptionName
    -> RDS m EventSubscription
addSourceIdentifierToSubscription src name =
    rdsQuery "AddSourceIdentifierToSubscription" params $
        element "EventSubscription" eventSubscriptionSink
  where
    params =
        [ "SourceIdentifier" |= src
        , "SubscriptionName" |= name
        ]
