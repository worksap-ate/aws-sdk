{-# LANGUAGE FlexibleContexts #-}
module AWS.RDS.Event
    ( describeEvents
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Conduit
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.XML.Types as XML

import AWS.Lib.Parser
import AWS.Lib.Query
import AWS.RDS.Internal
import AWS.RDS.Types
import AWS.Util (toText)

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

sourceTypeToText :: SourceType -> Text
sourceTypeToText SourceTypeDBInstance = "db-instance"
sourceTypeToText SourceTypeDBParameterGroup = "db-parameter-group"
sourceTypeToText SourceTypeDBSecurityGroup = "db-security-group"
sourceTypeToText SourceTypeDBSnapshot = "db-snapshot"

eventSink
    :: MonadThrow m
    => GLSink XML.Event m Event
eventSink = Event
    <$> getT "Message"
    <*> getT "SourceType"
    <*> listConsumer "EventCategories" (getT "EventCategory")
    <*> getT "Date"
    <*> getT "SourceIdentifier"
