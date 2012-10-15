module AWS.ELB.Types
    where

import Data.Text (Text)
import Data.Time (UTCTime)

data LoadBalancerDescription = LoadBalancerDescription
    { lbSecurityGroups :: [Text]
    , lbLoadBalancerName :: Text
    , lbCreatedTime :: UTCTime
    , lbHealthCheck :: HealthCheck
    , lbVPCId :: Maybe Text
    , lbListenerDescriptions :: [ListenerDescription]
    , lbInstances :: [Instance]
    , lbPolicies :: Policies
    , lbAvailabilityZones :: [Text]
    , lbCanonicalHostedZoneName :: Text
    , lbCanonicalHostedZoneNameID :: Text
    , lbScheme :: Text
    , lbSourceSecurityGroup :: Maybe SourceSecurityGroup
    , lbDNSName :: Text
    , lbBackendServerDescriptions :: [BackendServerDescription]
    , lbSubnets :: [Text]
    }
  deriving (Show)

data BackendServerDescription = BackendServerDescription
    { bsInstancePort :: Int
    , bsPolicyNames :: [Text]
    }
  deriving (Show)

data HealthCheck = HealthCheck
    { hcInterval :: Int
    , hcTarget :: Text
    , hcHealthyThreshold :: Int
    , hcTimeout :: Int
    , hcUnhealthyThreshold :: Int
    }
  deriving (Show)

data Instance = Instance
    { iInstanceId :: Text
    }
  deriving (Show)

data ListenerDescription = ListenerDescription
    { ldPolicyNames :: [Text]
    , ldListener :: Listener
    }
  deriving (Show)

data Listener = Listener
    { lProtocol :: Text
    , lLoadBalancerPort :: Int
    , lInstanceProtocol :: Text
    , lSSLCertificateId :: Maybe Text
    , lInstancePort :: Int
    }
  deriving (Show)

data Policies = Policies
    { pAppCookieStickinessPolicies :: [AppCookieStickinessPolicy]
    , pOtherPolicies :: [Text]
    , pLBCookieStickinessPolicies :: [LBCookieStickinessPolicy]
    }
  deriving (Show)

data AppCookieStickinessPolicy = AppCookieStickinessPolicy
    { acspCookieName :: Text
    , acspPolicyName :: Text
    }
  deriving (Show)

data LBCookieStickinessPolicy = LBCookieStickinessPolicy
    { lbcspCookieExpirationPeriod :: Integer
    , lbcspPolicyName :: Text
    }
  deriving (Show)

data SourceSecurityGroup = SourceSecurityGroup
    { ssgOwnerAlias :: Text
    , ssgGroupName :: Text
    }
  deriving (Show)
