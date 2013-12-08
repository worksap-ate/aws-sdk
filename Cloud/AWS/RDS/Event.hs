{-# LANGUAGE FlexibleContexts #-}

module Cloud.AWS.RDS.Event
    ( describeEvents
    , describeEventCategories
    ) where

import Control.Applicative
import Data.Conduit
import Data.Text (Text)
import Data.Time (UTCTime)

import Cloud.AWS.Lib.Parser.Unordered (SimpleXML, (.<), content)
import Cloud.AWS.Lib.Query
import Cloud.AWS.RDS.Internal
import Cloud.AWS.RDS.Types

describeEvents
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Text -- ^ SourceIdentifier
    -> Maybe SourceType -- ^ SourceType
    -> Maybe Int -- ^ Duration
    -> Maybe UTCTime -- ^ StartTime
    -> Maybe UTCTime -- ^ EndTime
    -> [Text] -- ^ EventCategories.member
    -> Maybe Text -- ^ Marker
    -> Maybe Int -- ^ MaxRecords
    -> RDS m [Event]
describeEvents sid stype d start end categories marker maxRecords =
    rdsQuery "DescribeEvents" params $
        elements "Event" eventSink
  where
    params =
        [ "SourceIdentifier" |=? sid
        , "SourceType" |=? stype
        , "Duration" |=? d
        , "StartTime" |=? start
        , "EndTime" |=? end
        , "EventCategories" |.+ "member" |.#= categories
        , "Marker" |=? marker
        , "MaxRecords" |=? maxRecords
        ]

eventSink
    :: (MonadThrow m, Applicative m)
    => SimpleXML -> m Event
eventSink xml = Event
    <$> xml .< "Message"
    <*> xml .< "SourceType"
    <*> elements' "EventCategories" "EventCategory" content xml
    <*> xml .< "Date"
    <*> xml .< "SourceIdentifier"

describeEventCategories
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe SourceType -- ^ SourceType
    -> RDS m [EventCategoriesMap]
describeEventCategories stype =
    rdsQuery "DescribeEventCategories" params $
        elements' "EventCategoriesMapList" "EventCategoriesMap" eventCategoriesMapSink
  where
    params = [ "SourceType" |=? stype ]

eventCategoriesMapSink
    :: (MonadThrow m, Applicative m)
    => SimpleXML -> m EventCategoriesMap
eventCategoriesMapSink xml = EventCategoriesMap
    <$> xml .< "SourceType"
    <*> elements' "EventCategories" "EventCategory" content xml
