{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.RDS.Types.Event
    ( Event(..)
    , SourceType(..)
    , EventCategoriesMap(..)
    ) where

import Cloud.AWS.Lib.FromText (deriveFromText, Text, UTCTime)
import Cloud.AWS.Lib.ToText

data Event = Event
    { eventMessage :: Text
    , eventSourceType :: SourceType
    , eventCategories :: [Text]
    , eventDate :: UTCTime
    , eventSourceIdentifier :: Text
    }
  deriving (Show, Eq)

data SourceType
    = SourceTypeDBInstance
    | SourceTypeDBParameterGroup
    | SourceTypeDBSecurityGroup
    | SourceTypeDBSnapshot
  deriving (Show, Read, Eq)

instance ToText SourceType where
    toText SourceTypeDBInstance = "db-instance"
    toText SourceTypeDBParameterGroup = "db-parameter-group"
    toText SourceTypeDBSecurityGroup = "db-security-group"
    toText SourceTypeDBSnapshot = "db-snapshot"

data EventCategoriesMap = EventCategoriesMap
    { eventCategoriesMapSourceType :: SourceType
    , eventCategoriesMapEventCategories :: [Text]
    }
  deriving (Show, Eq)

deriveFromText "SourceType"
    ["db-instance", "db-parameter-group", "db-security-group", "db-snapshot"]
