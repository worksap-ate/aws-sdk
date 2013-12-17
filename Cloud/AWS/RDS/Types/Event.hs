{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.RDS.Types.Event
    ( Event(..)
    , SourceType(..)
    , EventCategoriesMap(..)
    ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Cloud.AWS.Lib.FromText (deriveFromText)
import Cloud.AWS.Lib.ToText (deriveToText)

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

deriveToText "SourceType"
    ["db-instance", "db-parameter-group", "db-security-group", "db-snapshot"]

data EventCategoriesMap = EventCategoriesMap
    { eventCategoriesMapSourceType :: SourceType
    , eventCategoriesMapEventCategories :: [Text]
    }
  deriving (Show, Eq)

deriveFromText "SourceType"
    ["db-instance", "db-parameter-group", "db-security-group", "db-snapshot"]
