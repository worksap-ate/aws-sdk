module Cloud.AWS.RDS.Types.OptionGroup
    ( OptionGroup(..)
    , Option(..)
    , OptionGroupOption(..)
    , ModifyOptionGroupRequest(..)
    , OptionConfiguration(..)
    ) where

import Cloud.AWS.Lib.FromText (Text)
import Cloud.AWS.RDS.Types.DBInstance
    ( VpcSecurityGroupMembership
    , DBSecurityGroupMembership
    )

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
    , optionVpcSecurityGroupMemberships :: [VpcSecurityGroupMembership]
    , optionDBSecurityGroupMemberships :: [DBSecurityGroupMembership]
    }
  deriving (Show, Eq)

data OptionGroupOption = OptionGroupOption
    { optionGroupOptionMajorEngineVersion :: Text
    , optionGroupOptionPortRequired :: Bool
    , optionGroupOptionOptionsDependedOn :: [Text]
    , optionGroupOptionDescription :: Text
    , optionGroupOptionDefaultPort :: Maybe Int
    , optionGroupOptionName :: Text
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
    , optionConfigurationPort :: Int
    , optionConfigurationVpcSecurityGroupMemberships :: [Text]
    }
  deriving (Show, Eq)
