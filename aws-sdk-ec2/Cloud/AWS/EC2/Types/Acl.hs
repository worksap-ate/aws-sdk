{-# LANGUAGE TemplateHaskell #-}

module Cloud.AWS.EC2.Types.Acl
    ( IcmpTypeCode(..)
    , NetworkAcl(..)
    , NetworkAclAssociation(..)
    , NetworkAclEntry(..)
    , NetworkAclEntryRequest(..)
    , NetworkAclRuleAction(..)
    , PortRange(..)
    ) where

import Cloud.AWS.EC2.Types.Common (ResourceTag)
import Cloud.AWS.Lib.FromText (deriveFromText)
import Cloud.AWS.Lib.ToText (deriveToText)
import Data.Text (Text)
import Data.IP (AddrRange, IPv4)

data IcmpTypeCode = IcmpTypeCode
    { icmpTypeCodeCode :: Int
    , icmpTypeCodeType :: Int
    }
  deriving (Show, Read, Eq)

data NetworkAcl = NetworkAcl
    { networkAclId :: Text
    , networkAclVpcId :: Text
    , networkAclDefault :: Bool
    , networkAclEntrySet :: [NetworkAclEntry]
    , networkAclAssociationSet :: [NetworkAclAssociation]
    , networkAclTagSet :: [ResourceTag]
    }
  deriving (Show, Read, Eq)

data NetworkAclAssociation = NetworkAclAssociation
    { networkAclAssociationId :: Text
    , networkAclAssociationNetworkAclId :: Text
    , networkAclAssociationSubnetId :: Text
    }
  deriving (Show, Read, Eq)

data NetworkAclEntry = NetworkAclEntry
    { networkAclEntryRuleNumber :: Int
    , networkAclEntryProtocol :: Int
    , networkAclEntryRuleAction :: NetworkAclRuleAction
    , networkAclEntryEgress :: Bool
    , networkAclEntryCidrBlock :: AddrRange IPv4
    , networkAclEntryIcmpTypeCode :: Maybe IcmpTypeCode
    , networkAclEntryPortRange :: Maybe PortRange
    }
  deriving (Show, Read, Eq)

data NetworkAclEntryRequest = NetworkAclEntryRequest
    { networkAclEntryRequestNetworkAclId :: Text
    , networkAclEntryRequestRuleNumber :: Int
    , networkAclEntryRequestProtocol :: Int
      -- ^ Protocol Number <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xml>
    , networkAclEntryRequestRuleAction :: NetworkAclRuleAction
    , networkAclEntryRequestEgress :: Bool
    , networkAclEntryRequestCidrBlock :: AddrRange IPv4
    , networkAclEntryRequestIcmp :: Maybe IcmpTypeCode
    , networkAclEntryRequestPortRange :: Maybe PortRange
    }
  deriving (Show, Read, Eq)

data NetworkAclRuleAction
    = NetworkAclRuleActionAllow
    | NetworkAclRuleActionDeny
  deriving (Show, Read, Eq)

data PortRange = PortRange
    { portRangeFrom :: Int
    , portRangeTo :: Int
    }
  deriving (Show, Read, Eq)

deriveFromText "NetworkAclRuleAction" ["allow", "deny"]
deriveToText "NetworkAclRuleAction" ["allow", "deny"]
