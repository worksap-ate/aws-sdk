module AWS.RDS.Types.OptionGroup
    ( OptionGroup(..)
    , Option(..)
    ) where

import AWS.Lib.FromText (Text)
import AWS.RDS.Types.DBInstance
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
