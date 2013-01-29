{-# LANGUAGE TemplateHaskell #-}

module AWS.RDS.Types.Event
    ( Event(..)
    , SourceType(..)
    ) where

import AWS.Lib.FromText (deriveFromText, Text, UTCTime)

data Event = Event
    { eventMessage :: Text
    , eventSourceType :: SourceType
    , eventEventCategories :: [Text]
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

deriveFromText "SourceType" ["db-instance", "db-parameter-group", "db-security-group", "db-snapshot"]
