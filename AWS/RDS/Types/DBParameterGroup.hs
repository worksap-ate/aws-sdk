module AWS.RDS.Types.DBParameterGroup
    ( DBParameterGroup(..)
    , Parameter(..)
    , ModifyParameter(..)
    , ApplyMethod(..)
    , ResetParameterRequest(..)
    , ResetParameter(..)
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

data ModifyParameter = ModifyParameter
    { modifyParameterName :: Text
    , modifyParameterValue :: Text
    , modifyParameterApplyMethod :: ApplyMethod
    }
  deriving (Show, Eq)

data ApplyMethod
    = ApplyMethodImmediate
    | ApplyMethodPendingReboot
  deriving (Show, Eq)

data ResetParameterRequest
    = ResetAllParameters
    | ResetParameters [ResetParameter]
  deriving (Show, Eq)

data ResetParameter = ResetParameter
    { resetParameterName :: Text
    , resetParameterApplyMethod :: ApplyMethod
    }
  deriving (Show, Eq)
