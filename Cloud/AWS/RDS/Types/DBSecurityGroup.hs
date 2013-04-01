{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.RDS.Types.DBSecurityGroup
    ( DBSecurityGroup(..)
    , EC2SecurityGroup(..)
    , EC2SecurityGroupStatus(..)
    , IPRange(..)
    , IPRangeStatus(..)
    ) where

import Cloud.AWS.Lib.FromText (deriveFromText, AddrRange, IPv4, Text)

data DBSecurityGroup = DBSecurityGroup
    { dbSecurityGroupEC2SecurityGroups :: [EC2SecurityGroup]
    , dbSecurityGroupDescription :: Text
    , dbSecurityGroupIPRanges :: [IPRange]
    , dbSecurityGroupVpcId :: Maybe Text
    , dbSecurityGroupOwnerId :: Text
    , dbSecurityGroupName :: Text
    }
  deriving (Show, Eq)

data EC2SecurityGroup = EC2SecurityGroup
    { ec2SecurityGroupStatus :: EC2SecurityGroupStatus
    , ec2SecurityGroupName :: Text
    , ec2SecurityGroupOwnerId :: Maybe Text
    , ec2SecurityGroupId :: Maybe Text
    }
  deriving (Show, Eq)

data EC2SecurityGroupStatus
    = EC2SecurityGroupStatusAuthorizing
    | EC2SecurityGroupStatusAuthorized
    | EC2SecurityGroupStatusRevoking
    | EC2SecurityGroupStatusRevoked
  deriving (Show, Read, Eq)

data IPRange = IPRange
    { ipRangeCidrIp :: AddrRange IPv4
    , ipRangeStatus :: IPRangeStatus
    }
  deriving (Show, Eq)

data IPRangeStatus
    = IPRangeStatusAuthorizing
    | IPRangeStatusAuthorized
    | IPRangeStatusRevoking
    | IPRangeStatusRevoked
  deriving (Show, Read, Eq)

deriveFromText "EC2SecurityGroupStatus"
    ["authorizing", "authorized", "revoking", "revoked"]
deriveFromText "IPRangeStatus"
    ["authorizing", "authorized", "revoking", "revoked"]
