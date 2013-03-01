module AWS.RDS.Types.DBParameterGroup
    ( DBParameterGroup(..)
    , Parameter(..)
    ) where

import AWS.Lib.FromText (Text)

data DBParameterGroup = DBParameterGroup
    { dbParameterGroupFamily :: Text
    , dbParameterGroupDescription :: Text
    , dbParameterGroupName :: Text
    }
  deriving (Show, Eq)

data Parameter = Parameter
    { parameterValue :: Maybe Text
    , parameterDataType :: Text
    , parameterSource :: Text
    , parameterIsModifiable :: Bool
    , parameterDescription :: Text
    , parameterApplyType :: Text
    , parameterAllowedValues :: Maybe Text
    , parameterName :: Text
    , parameterMinimumEngineVersion :: Maybe Text
    }
  deriving (Show, Eq)
