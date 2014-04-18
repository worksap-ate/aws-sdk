module Cloud.AWS.RDS.Types.OptionGroup
    ( OptionGroup(..)
    , Option(..)
    , OptionGroupOption(..)
    , ModifyOptionGroupRequest(..)
    , OptionConfiguration(..)
    , OptionSetting(..)
    , OptionGroupOptionSetting(..)
    ) where

import Cloud.AWS.RDS.Types.DBInstance
    ( VpcSecurityGroupMembership
    , DBSecurityGroupMembership
    )
import Data.Text (Text)

data OptionGroup = OptionGroup
    { optionGroupAllowsVpcAndNonVpcInstanceMemberships :: Bool
    , optionGroupMajorEngineVersion :: Text
    , optionGroupName :: Text
    , optionGroupVpcId :: Maybe Text
    , optionGroupEngineName :: Text
    , optionGroupDescription :: Text
    , optionGroupOption :: [Option]
    }
  deriving (Show, Eq)

data Option = Option
    { optionPort :: Int
    , optionName :: Text
    , optionDescription :: Text
    , optionPersistent :: Bool
    , optionSettings :: [OptionSetting]
    , optionVpcSecurityGroupMemberships :: [VpcSecurityGroupMembership]
    , optionDBSecurityGroupMemberships :: [DBSecurityGroupMembership]
    }
  deriving (Show, Eq)

data OptionGroupOption = OptionGroupOption
    { optionGroupOptionMajorEngineVersion :: Text
    , optionGroupOptionPersistent :: Bool
    , optionGroupOptionPortRequired :: Bool
    , optionGroupOptionOptionsDependedOn :: [Text]
    , optionGroupOptionDescription :: Text
    , optionGroupOptionDefaultPort :: Maybe Int
    , optionGroupOptionName :: Text
    , optionGroupOptionSettings :: [OptionGroupOptionSetting]
    , optionGroupOptionEngineName :: Text
    , optionGroupOptionMinimumRequiredMinorEngineVersion :: Text
    }
  deriving (Show, Eq)

data ModifyOptionGroupRequest
    = OptionsToInclude [OptionConfiguration]
    | OptionsToRemove [Text]
  deriving (Show, Eq)

data OptionConfiguration = OptionConfiguration
    { optionConfigurationDBSecurityGroupMemberships :: [Text]
    , optionConfigurationOptionName :: Text
    , optionConfigurationOptionSettings :: [OptionSetting]
    , optionConfigurationPort :: Maybe Int
    , optionConfigurationVpcSecurityGroupMemberships :: [Text]
    }
  deriving (Show, Eq)

data OptionSetting = OptionSetting
    { optionSettingAllowedValues :: Text
    , optionSettingApplyType :: Text
    , optionSettingDataType :: Text
    , optionSettingDefaultValue :: Text
    , optionSettingDescription :: Text
    , optionSettingIsCollection :: Bool
    , optionSettingIsModifiable :: Bool
    , optionSettingName :: Text
    , optionSettingValue :: Text
    }
  deriving (Show, Eq)

data OptionGroupOptionSetting = OptionGroupOptionSetting
    { optionGroupOptionSettingAllowedValues :: Text
    , optionGroupOptionSettingApplyType :: Text
    , optionGroupOptionSettingDefaultValue :: Text
    , optionGroupOptionSettingIsModifiable :: Bool
    , optionGroupOptionSettingSettingDescription :: Text
    , optionGroupOptionSettingSettingName :: Text
    }
  deriving (Show, Eq)
