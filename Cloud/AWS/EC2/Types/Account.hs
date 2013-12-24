{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.EC2.Types.Account
    ( AccountAttribute(..)
    , AttributeName(..)
    ) where

import Cloud.AWS.Lib.FromText (deriveFromText)
import Cloud.AWS.Lib.ToText (deriveToText)
import Data.Text (Text)

data AccountAttribute = AccountAttribute
    { accountAttribute :: AttributeName
    , accountValues :: [Text]
    }
  deriving (Show, Read, Eq)

data AttributeName
    = AttributeNameSupportedPlatforms
    | AttributeNameDefaultVpc
    | AttributeNameVpcMaxSecurityGroupsPerInterface
    | AttributeNameMaxInstances
    | AttributeNameMaxElasticIPs
    | AttributeNameVpcMaxElasticIPs
  deriving (Show, Read, Eq)

deriveFromText "AttributeName"
    [ "supported-platforms"
    , "default-vpc"
    , "vpc-max-security-groups-per-interface"
    , "max-instances"
    , "max-elastic-ips"
    , "vpc-max-elastic-ips"
    ]
deriveToText "AttributeName"
    [ "supported-platforms"
    , "default-vpc"
    , "vpc-max-security-groups-per-interface"
    , "max-instances"
    , "max-elastic-ips"
    , "vpc-max-elastic-ips"
    ]
