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
  deriving (Show, Eq)

data BackendServerDescription = BackendServerDescription
    { bsInstancePort :: Int
    , bsPolicyNames :: [Text]
    }
  deriving (Show, Eq)

data HealthCheck = HealthCheck
    { hcInterval :: Int
    , hcTarget :: Text
    , hcHealthyThreshold :: Int
    , hcTimeout :: Int
    , hcUnhealthyThreshold :: Int
    }
  deriving (Show, Eq)

data Instance = Instance
    { iInstanceId :: Text
    }
  deriving (Show, Eq)

data ListenerDescription = ListenerDescription
    { ldPolicyNames :: [Text]
    , ldListener :: Listener
    }
  deriving (Show, Eq)

data Listener = Listener
    { lProtocol :: Text
    , lLoadBalancerPort :: Int
    , lInstanceProtocol :: Text
    , lSSLCertificateId :: Maybe Text
    , lInstancePort :: Int
    }
  deriving (Show, Eq)

data Policies = Policies
    { pAppCookieStickinessPolicies :: [AppCookieStickinessPolicy]
    , pOtherPolicies :: [Text]
    , pLBCookieStickinessPolicies :: [LBCookieStickinessPolicy]
    }
  deriving (Show, Eq)

data AppCookieStickinessPolicy = AppCookieStickinessPolicy
    { acspCookieName :: Text
    , acspPolicyName :: Text
    }
  deriving (Show, Eq)

data LBCookieStickinessPolicy = LBCookieStickinessPolicy
    { lbcspCookieExpirationPeriod :: Integer
    , lbcspPolicyName :: Text
    }
  deriving (Show, Eq)

data SourceSecurityGroup = SourceSecurityGroup
    { ssgOwnerAlias :: Text
    , ssgGroupName :: Text
    }
  deriving (Show, Eq)
