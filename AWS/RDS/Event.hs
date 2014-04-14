{-# LANGUAGE FlexibleContexts, CPP #-}

module AWS.RDS.Event
    ( describeEvents
    , describeEventCategories
    ) where

import Control.Applicative ((<$>), (<*>))
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)
#endif
import Data.Conduit
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.XML.Types as XML

import AWS.Lib.Parser
import AWS.Lib.Query
import AWS.RDS.Internal
import AWS.RDS.Types
import AWS.Util (toText)
import Debug.Trace

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
        , "SourceType" |=? sourceTypeToText <$> stype
        , "Duration" |=? toText <$> d
        , "StartTime" |=? toText <$> start
        , "EndTime" |=? toText <$> end
        , "EventCategories" |.+ "member" |.#= categories
        , "Marker" |=? marker
        , "MaxRecords" |=? toText <$> maxRecords
        ]

eventSink
    :: MonadThrow m
    => Consumer XML.Event m Event
eventSink = Event
    <$> do
        a <- getT "Message"
        traceShow a $ return a
    <*> getT "SourceType"
    <*> elements' "EventCategories" "EventCategory" text
    <*> getT "Date"
    <*> getT "SourceIdentifier"

describeEventCategories
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe SourceType -- ^ SourceType
    -> RDS m [EventCategoriesMap]
describeEventCategories stype =
    rdsQuery "DescribeEventCategories" params $
        elements' "EventCategoriesMapList" "EventCategoriesMap" eventCategoriesMapSink
  where
    params = [ "SourceType" |=? sourceTypeToText <$> stype ]

eventCategoriesMapSink
    :: MonadThrow m
    => Consumer XML.Event m EventCategoriesMap
eventCategoriesMapSink = EventCategoriesMap
    <$> getT "SourceType"
    <*> elements' "EventCategories" "EventCategory" text
