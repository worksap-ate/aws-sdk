{-# LANGUAGE TemplateHaskell #-}

module AWS.RDS.Types.Event
    ( Event(..)
    , SourceType(..)
    , EventCategoriesMap(..)
    ) where

import AWS.Lib.FromText (deriveFromText, Text, UTCTime)

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

data EventCategoriesMap = EventCategoriesMap
    { eventCategoriesMapSourceType :: SourceType
    , eventCategoriesMapEventCategories :: [Text]
    }
  deriving (Show, Eq)

deriveFromText "SourceType"
    ["db-instance", "db-parameter-group", "db-security-group", "db-snapshot"]
