{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.RDS.Types.DBParameterGroup
    ( DBParameterGroup(..)
    , Parameter(..)
    , ModifyParameter(..)
    , ApplyMethod(..)
    , ResetParameterRequest(..)
    , ResetParameter(..)
    , DBEngineVersion(..)
    , CharacterSet(..)
    , EngineDefaults(..)
    ) where

import Data.Text (Text)
import Cloud.AWS.Lib.ToText (deriveToText)

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

deriveToText "ApplyMethod"
    [ "immediate", "pending-reboot" ]

data ResetParameterRequest
    = ResetAllParameters
    | ResetParameters [ResetParameter]
  deriving (Show, Eq)

data ResetParameter = ResetParameter
    { resetParameterName :: Text
    , resetParameterApplyMethod :: ApplyMethod
    }
  deriving (Show, Eq)

data DBEngineVersion = DBEngineVersion
    { dbEngineversionDBParameterGroupFamily :: Text
    , dbEngineVersionEngine :: Text
    , dbEngineVersionSupportedCharacterSets :: [CharacterSet]
    , dbEngineVersionDBEngineDescription :: Text
    , dbEngineVersionDefaultCharacterSet :: Maybe CharacterSet
    , dbEngineVersionEngineVersion :: Text
    , dbEngineVersionDBEngineVersionDescription :: Text
    }
  deriving (Show, Eq)

data CharacterSet = CharacterSet
    { characterSetName :: Text
    , characterSetDescription :: Text
    }
  deriving (Show, Eq)

data EngineDefaults = EngineDefaults
    { engineDefaultsDBParameterGroupFamily :: Text
    , engineDefaultsParameters :: [Parameter]
    }
  deriving (Show, Eq)
