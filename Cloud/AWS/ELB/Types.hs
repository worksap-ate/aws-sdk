{-# LANGUAGE TemplateHaskell #-}
module Cloud.AWS.ELB.Types
    where

import Data.Text (Text)
import Data.Time (UTCTime)
import Cloud.AWS.Lib.FromText (deriveFromText)

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
    { lbCookieStickinessPolicyPolicyName :: Text
    , lbCookieStickinessPolicyCookieExpirationPeriod :: Maybe Integer
    }
  deriving (Show, Eq)

data SourceSecurityGroup = SourceSecurityGroup
    { sourceSecurityGroupOwnerAlias :: Text
    , sourceSecurityGroupGroupName :: Text
    }
  deriving (Show, Eq)

data PolicyDescription = PolicyDescription
    { policyName :: Text
    , policyTypeName :: Text
    , policyAttributes :: [PolicyAttribute]
    }
  deriving (Show, Eq)

data PolicyAttribute = PolicyAttribute
    { policyAttributeName :: Text
    , policyAttributeValue :: Text
    }
  deriving (Show, Eq)

data PolicyType = PolicyType
    { policyTypeAttributeTypes :: [PolicyAttributeType]
    , policyTypeTypeName :: Text
    , policyTypeDescription :: Text
    }
  deriving (Show, Eq)

data PolicyAttributeType = PolicyAttributeType
    { policyAttributeTypeAttributeName :: Text
    , policyAttributeTypeAttributeType :: Text
    , policyAttributeTypeDefaultValue :: Maybe Text
    , policyAttributeTypeCardinality :: PolicyAttributeCardinality
    , policyAttributeTypeDescription :: Maybe Text
    }
  deriving (Show, Eq)

data PolicyAttributeCardinality
    = PolicyAttributeCardinalityOne
    | PolicyAttributeCardinalityZeroOrOne
    | PolicyAttributeCardinalityZeroOrMore
    | PolicyAttributeCardinalityOneOrMore
  deriving (Show, Eq, Read)

deriveFromText "PolicyAttributeCardinality" ["ONE", "ZERO_OR_ONE", "ZERO_OR_MORE", "ONE_OR_MORE"]

data InstanceState = InstanceState
    { instanceStateDescription :: Text
    , instanceStateInstanceId :: Text
    , instanceStateState :: InstanceStateState
    , instanceStateReasonCode :: Maybe Text
    }
  deriving (Show, Eq)

data InstanceStateState
    = InstanceStateInService
    | InstanceStateOutOfService
    deriving (Show, Eq, Read)

deriveFromText "InstanceStateState" ["InService", "OutOfService"]
