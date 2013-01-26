module AWS.ELB.Types
    where

import AWS.Lib.FromText

data LoadBalancer = LoadBalancer
    { loadBalancerSecurityGroups :: [Text]
    , loadBalancerCreatedTime :: UTCTime
    , loadBalancerLoadBalancerName :: Text
    , loadBalancerHealthCheck :: HealthCheck
    , loadBalancerVPCId :: Maybe Text
    , loadBalancerListenerDescriptions :: [ListenerDescription]
    , loadBalancerInstances :: [Instance]
    , loadBalancerPolicies :: Policies
    , loadBalancerAvailabilityZones :: [Text]
    , loadBalancerCanonicalHostedZoneName :: Maybe Text
    , loadBalancerCanonicalHostedZoneNameID :: Maybe Text
    , loadBalancerScheme :: Text
    , loadBalancerSourceSecurityGroup :: Maybe SourceSecurityGroup
    , loadBalancerDNSName :: Text
    , loadBalancerBackendServerDescriptions
        :: [BackendServerDescription]
    , loadBalancerSubnets :: [Text]
    }
  deriving (Show, Eq)

data BackendServerDescription = BackendServerDescription
    { backendServerInstancePort :: Int
    , backendServerPolicyNames :: [Text]
    }
  deriving (Show, Eq)

data HealthCheck = HealthCheck
    { healthCheckInterval :: Int
    , healthCheckTarget :: Text
    , healthCheckHealthyThreshold :: Int
    , healthCheckTimeout :: Int
    , healthCheckUnhealthyThreshold :: Int
    }
  deriving (Show, Eq)

data Instance = Instance
    { instanceId :: Text
    }
  deriving (Show, Eq)

data ListenerDescription = ListenerDescription
    { listenerDescriptionPolicyNames :: [Text]
    , listenerDescriptionListener :: Listener
    }
  deriving (Show, Eq)

data Listener = Listener
    { listenerProtocol :: Text
    , listenerLoadBalancerPort :: Int
    , listenerInstanceProtocol :: Text
    , listenerSSLCertificateId :: Maybe Text
    , listenerInstancePort :: Int
    }
  deriving (Show, Eq)

data Policies = Policies
    { policiesAppCookieStickinessPolicies :: [AppCookieStickinessPolicy]
    , policiesOtherPolicies :: [Text]
    , policiesLBCookieStickinessPolicies :: [LBCookieStickinessPolicy]
    }
  deriving (Show, Eq)

data AppCookieStickinessPolicy = AppCookieStickinessPolicy
    { appCookieStickinessPolicyCookieName :: Text
    , appCookieStickinessPolicyPolicyName :: Text
    }
  deriving (Show, Eq)

data LBCookieStickinessPolicy = LBCookieStickinessPolicy
    { lbCookieStickinessPolicyCookieExpirationPeriod :: Integer
    , lbCookieStickinessPolicyPolicyName :: Text
    }
  deriving (Show, Eq)

data SourceSecurityGroup = SourceSecurityGroup
    { sourceSecurityGroupOwnerAlias :: Text
    , sourceSecurityGroupGroupName :: Text
    }
  deriving (Show, Eq)
