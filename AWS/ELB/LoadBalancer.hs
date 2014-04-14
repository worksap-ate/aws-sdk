{-# LANGUAGE FlexibleContexts, RankNTypes, RecordWildCards, CPP #-}

module AWS.ELB.LoadBalancer
    ( describeLoadBalancers
    , createLoadBalancer
    , deleteLoadBalancer
    , attachLoadBalancerToSubnets
    , detachLoadBalancerFromSubnets
    , applySecurityGroupsToLoadBalancer
    , registerInstancesWithLoadBalancer
    , deregisterInstancesFromLoadBalancer
    , setLoadBalancerListenerSSLCertificate
    , createLoadBalancerListeners
    , deleteLoadBalancerListeners
    , describeLoadBalancerPolicies
    , describeLoadBalancerPolicyTypes
    , createLoadBalancerPolicy
    , deleteLoadBalancerPolicy
    , describeInstanceHealth
    , configureHealthCheck
    , enableAvailabilityZonesForLoadBalancer
    , disableAvailabilityZonesForLoadBalancer
    , createLBCookieStickinessPolicy
    , createAppCookieStickinessPolicy
    , setLoadBalancerPoliciesOfListener
    , setLoadBalancerPoliciesForBackendServer
    ) where

import Data.Text (Text)
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)
#endif
import Data.Conduit
import Control.Applicative
import Data.XML.Types (Event(..))

import AWS.Lib.Parser
import AWS.Lib.Query

import AWS.ELB.Types
import AWS.ELB.Internal

import AWS.Util (toText)

describeLoadBalancers
    :: (MonadBaseControl IO m, MonadResource m)
    => [Text] -- ^ LoadBalancerNames
    -> Maybe Text -- ^ Marker
    -> ELB m [LoadBalancer]
describeLoadBalancers lbs marker =
    elbQuery "DescribeLoadBalancers" params sinkLoadBalancers
  where
    params =
        [ "LoadBalancerNames.member" |.#= lbs
        , "Marker" |=? marker
        ]

sinkLoadBalancers :: MonadThrow m
    => Consumer Event m [LoadBalancer]
sinkLoadBalancers = members "LoadBalancerDescriptions" $
    LoadBalancer
    <$> members "SecurityGroups" text
    <*> getT "CreatedTime"
    <*> getT "LoadBalancerName"
    <*> element "HealthCheck" sinkHealthCheck
    <*> getT "VPCId"
    <*> members "ListenerDescriptions"
        (ListenerDescription
        <$> members "PolicyNames" text
        <*> element "Listener"
            (Listener
            <$> getT "Protocol"
            <*> getT "LoadBalancerPort"
            <*> getT "InstanceProtocol"
            <*> getT "SSLCertificateId"
            <*> getT "InstancePort"
            )
        )
    <*> members "Instances" sinkInstance
    <*> element "Policies"
        (Policies
        <$> members "AppCookieStickinessPolicies"
            (AppCookieStickinessPolicy
            <$> getT "CookieName"
            <*> getT "PolicyName"
            )
        <*> members "OtherPolicies" text
        <*> members "LBCookieStickinessPolicies"
            (LBCookieStickinessPolicy
            <$> getT "PolicyName"
            <*> getT "CookieExpirationPeriod"
            )
        )
    <*> members "AvailabilityZones" text
    <*> getT "CanonicalHostedZoneName"
    <*> getT "CanonicalHostedZoneNameID"
    <*> getT "Scheme"
    <*> elementM "SourceSecurityGroup"
        (SourceSecurityGroup
        <$> getT "OwnerAlias"
        <*> getT "GroupName"
        )
    <*> getT "DNSName"
    <*> members "BackendServerDescriptions"
        (BackendServerDescription
        <$> getT "InstancePort"
        <*> members "PolicyNames" text
        )
    <*> members "Subnets" text

sinkInstance :: MonadThrow m => Consumer Event m Instance
sinkInstance = Instance <$> getT "InstanceId"

sinkHealthCheck :: MonadThrow m => Consumer Event m HealthCheck
sinkHealthCheck =
    HealthCheck
    <$> getT "Interval"
    <*> getT "Target"
    <*> getT "HealthyThreshold"
    <*> getT "Timeout"
    <*> getT "UnhealthyThreshold"

createLoadBalancer
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ LoadBalancerName
    -> [Listener] -- ^ Listeners
    -> [Text] -- ^ AvailabilityZones
    -> Maybe Text -- ^ Scheme
    -> [Text] -- ^ SecurityGroups
    -> [Text] -- ^ Subnets
    -> ELB m Text -- return DNSName
createLoadBalancer name listeners zones scheme groups subnets =
    elbQuery "CreateLoadBalancer" params $ getT "DNSName"
  where
    params =
        [ "LoadBalancerName" |= name
        , "Listeners.member" |.#. listeners'
        , "AvailabilityZones.member" |.#= zones
        , "Scheme" |=? scheme
        , "SecurityGroups.member" |.#= groups
        , "Subnets.member" |.#= subnets
        ]
    listeners' = flip map listeners $
        \(Listener prot lbport iprot cert iport) ->
            [ "Protocol" |= prot
            , "LoadBalancerPort" |= toText lbport
            , "InstanceProtocol" |= iprot
            , "SSLCertificateId" |=? cert
            , "InstancePort" |= toText iport
            ]

deleteLoadBalancer
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ LoadBalancerName
    -> ELB m ()
deleteLoadBalancer name = elbQuery "DeleteLoadBalancer" params $
    getT_ "DeleteLoadBalancerResult"
  where
    params = ["LoadBalancerName" |= name]

attachLoadBalancerToSubnets
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The name associated with the LoadBalancer.
    -> [Text] -- ^ A list of subnet IDs to add for the LoadBalancer.
    -> ELB m [Text] -- ^ A list of subnet IDs added for the LoadBalancer.
attachLoadBalancerToSubnets name subnets =
    elbQuery "AttachLoadBalancerToSubnets" params $ members "Subnets" text
  where
    params =
        [ "LoadBalancerName" |= name
        , "Subnets.member" |.#= subnets
        ]

detachLoadBalancerFromSubnets
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The name associated with the LoadBalancer to be detached.
    -> [Text] -- ^ A list of subnet IDs to remove from the set of configured subnets for the LoadBalancer.
    -> ELB m [Text] -- ^ A list of subnet IDs removed from the configured set of subnets for the LoadBalancer.
detachLoadBalancerFromSubnets name subnets =
    elbQuery "DetachLoadBalancerFromSubnets" params $ members "Subnets" text
  where
    params =
        [ "LoadBalancerName" |= name
        , "Subnets.member" |.#= subnets
        ]

applySecurityGroupsToLoadBalancer
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The name associated with the LoadBalancer.
    -> [Text] -- ^ A list of security group IDs to associate with your LoadBalancer in VPC.
    -> ELB m [Text] -- ^ A list of security group IDs associated with your LoadBalancer.
applySecurityGroupsToLoadBalancer name sgs =
    elbQuery "ApplySecurityGroupsToLoadBalancer" params $ members "SecurityGroups" text
  where
    params =
        [ "LoadBalancerName" |= name
        , "SecurityGroups.member" |.#= sgs
        ]

registerInstancesWithLoadBalancer
    :: (MonadBaseControl IO m, MonadResource m)
    => [Text] -- ^ A list of instance IDs that should be registered with the LoadBalancer.
    -> Text -- ^ The name associated with the LoadBalancer.
    -> ELB m [Instance]
registerInstancesWithLoadBalancer insts name =
    elbQuery "RegisterInstancesWithLoadBalancer" params $ members "Instances" sinkInstance
  where
    params =
        [ "Instances.member" |.#. map toInstanceParam insts
        , "LoadBalancerName" |= name
        ]

toInstanceParam :: Text -> [QueryParam]
toInstanceParam inst = ["InstanceId" |= inst ]

deregisterInstancesFromLoadBalancer
    :: (MonadBaseControl IO m, MonadResource m)
    => [Text] -- ^ A list of EC2 instance IDs consisting of all instances to be deregistered.
    -> Text -- ^ A list of EC2 instance IDs consisting of all instances to be deregistered.
    -> ELB m [Instance]
deregisterInstancesFromLoadBalancer insts name =
    elbQuery "DeregisterInstancesFromLoadBalancer" params $ members "Instances" sinkInstance
  where
    params =
        [ "Instances.member" |.#. map toInstanceParam insts
        , "LoadBalancerName" |= name
        ]

setLoadBalancerListenerSSLCertificate
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The name of the the LoadBalancer.
    -> Int -- ^ The port that uses the specified SSL certificate.
    -> Text -- ^ The ID of the SSL certificate chain to use.
    -> ELB m ()
setLoadBalancerListenerSSLCertificate lb port cert =
    elbQuery "SetLoadBalancerListenerSSLCertificate" params $ getT_ "SetLoadBalancerListenerSSLCertificateResult"
  where
    params =
        [ "LoadBalancerName" |= lb
        , "LoadBalancerPort" |= toText port
        , "SSLCertificateId" |= cert
        ]

createLoadBalancerListeners
    :: (MonadBaseControl IO m, MonadResource m)
    => [Listener] -- ^ A list of Listeners
    -> Text -- ^ The name of the LoadBalancer.
    -> ELB m ()
createLoadBalancerListeners listeners lb =
    elbQuery "CreateLoadBalancerListeners" params $ getT_ "CreateLoadBalancerListenersResult"
  where
    params =
        [ "Listeners.member" |.#. map toListenerParam listeners
        , "LoadBalancerName" |= lb
        ]

toListenerParam :: Listener -> [QueryParam]
toListenerParam Listener{..} =
    [ "Protocol" |= listenerProtocol
    , "LoadBalancerPort" |= toText listenerLoadBalancerPort
    , "InstanceProtocol" |= listenerInstanceProtocol
    , "SSLCertificateId" |=? listenerSSLCertificateId
    , "InstancePort" |= toText listenerInstancePort
    ]

deleteLoadBalancerListeners
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The mnemonic name associated with the LoadBalancer.
    -> [Int] -- ^ The client port number(s) of the LoadBalancerListener(s) to be removed.
    -> ELB m ()
deleteLoadBalancerListeners lb ports =
    elbQuery "DeleteLoadBalancerListeners" params $ getT_ "DeleteLoadBalancerListenersResult"
  where
    params =
        [ "LoadBalancerName" |= lb
        , "LoadBalancerPorts.member" |.#= map toText ports
        ]

describeLoadBalancerPolicies
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Text -- ^ The mnemonic name associated with the LoadBalancer.
    -> [Text] -- ^ The names of LoadBalancer policies you've created or Elastic Load Balancing sample policy names.
    -> ELB m [PolicyDescription]
describeLoadBalancerPolicies mlb policies =
    elbQuery "DescribeLoadBalancerPolicies" params $ members "PolicyDescriptions" sinkPolicyDescription
  where
    params =
        [ "LoadBalancerName" |=? mlb
        , "PolicyNames.member" |.#= policies
        ]

sinkPolicyDescription :: MonadThrow m => Consumer Event m PolicyDescription
sinkPolicyDescription =
    PolicyDescription
    <$> getT "PolicyName"
    <*> getT "PolicyTypeName"
    <*> members "PolicyAttributeDescriptions" sinkPolicyAttribute

sinkPolicyAttribute :: MonadThrow m => Consumer Event m PolicyAttribute
sinkPolicyAttribute =
    PolicyAttribute
    <$> getT "AttributeName"
    <*> getT "AttributeValue"

describeLoadBalancerPolicyTypes
    :: (MonadBaseControl IO m, MonadResource m)
    => [Text] -- ^ Specifies the name of the policy types.
    -> ELB m [PolicyType]
describeLoadBalancerPolicyTypes typeNames =
    elbQuery "DescribeLoadBalancerPolicyTypes" params $ members "PolicyTypeDescriptions" sinkPolicyType
  where
    params = ["PolicyTypeNames.member" |.#= typeNames]

sinkPolicyType :: MonadThrow m => Consumer Event m PolicyType
sinkPolicyType =
    PolicyType
    <$> members "PolicyAttributeTypeDescriptions" sinkPolicyAttributeType
    <*> getT "PolicyTypeName"
    <*> getT "Description"

sinkPolicyAttributeType :: MonadThrow m => Consumer Event m PolicyAttributeType
sinkPolicyAttributeType =
    PolicyAttributeType
    <$> getT "AttributeName"
    <*> getT "AttributeType"
    <*> getT "DefaultValue"
    <*> getT "Cardinality"
    <*> getT "Description"

createLoadBalancerPolicy
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The name associated with the LoadBalancer for which the policy is being created.
    -> [PolicyAttribute] -- ^ A list of attributes associated with the policy being created.
    -> Text -- ^ The name of the LoadBalancer policy being created.
    -> Text -- ^ The name of the base policy type being used to create this policy.
    -> ELB m ()
createLoadBalancerPolicy lb attrs name typeName =
    elbQuery "CreateLoadBalancerPolicy" params $ getT_ "CreateLoadBalancerPolicyResult"
  where
    params =
        [ "LoadBalancerName" |= lb
        , "PolicyAttributes.member" |.#. map toAttributeParams attrs
        , "PolicyName" |= name
        , "PolicyTypeName" |= typeName
        ]

toAttributeParams :: PolicyAttribute -> [QueryParam]
toAttributeParams PolicyAttribute{..} =
    [ "AttributeName" |= policyAttributeName
    , "AttributeValue" |= policyAttributeValue
    ]

deleteLoadBalancerPolicy
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The mnemonic name associated with the LoadBalancer.
    -> Text -- ^ The mnemonic name for the policy being deleted.
    -> ELB m ()
deleteLoadBalancerPolicy lb policyName =
    elbQuery "DeleteLoadBalancerPolicy" params $ getT_ "DeleteLoadBalancerPolicyResult"
  where
    params =
        [ "LoadBalancerName" |= lb
        , "PolicyName" |= policyName
        ]

describeInstanceHealth
    :: (MonadBaseControl IO m, MonadResource m)
    => [Text] -- ^ A list of instance IDs whose states are being queried.
    -> Text -- ^ The name associated with the LoadBalancer.
    -> ELB m [InstanceState]
describeInstanceHealth insts lb =
    elbQuery "DescribeInstanceHealth" params $ members "InstanceStates" sinkInstanceState
  where
    params =
        [ "Instances.member" |.#. map toInstanceParam insts
        , "LoadBalancerName" |= lb
        ]

sinkInstanceState :: MonadThrow m => Consumer Event m InstanceState
sinkInstanceState =
    InstanceState
    <$> getT "Description"
    <*> getT "InstanceId"
    <*> getT "State"
    <*> getT "ReasonCode"

configureHealthCheck
    :: (MonadBaseControl IO m, MonadResource m)
    => HealthCheck -- ^ A structure containing the configuration information for the new healthcheck.
    -> Text -- ^ The mnemonic name associated with the LoadBalancer.
    -> ELB m HealthCheck
configureHealthCheck hc lb =
    elbQuery "ConfigureHealthCheck" params $ element "HealthCheck" sinkHealthCheck
  where
    params =
        [ "HealthCheck" |. toHealthCheckParams hc
        , "LoadBalancerName" |= lb
        ]

toHealthCheckParams :: HealthCheck -> [QueryParam]
toHealthCheckParams HealthCheck{..} =
    [ "HealthyThreshold" |= toText healthCheckHealthyThreshold
    , "Interval" |= toText healthCheckInterval
    , "Target" |= healthCheckTarget
    , "Timeout" |= toText healthCheckTimeout
    , "UnhealthyThreshold" |= toText healthCheckUnhealthyThreshold
    ]

enableAvailabilityZonesForLoadBalancer
    :: (MonadBaseControl IO m, MonadResource m)
    => [Text] -- ^ A list of new Availability Zones for the LoadBalancer.
    -> Text -- ^ The name associated with the LoadBalancer.
    -> ELB m [Text] -- ^ An updated list of Availability Zones for the LoadBalancer.
enableAvailabilityZonesForLoadBalancer zones lb =
    elbQuery "EnableAvailabilityZonesForLoadBalancer" params $ members "AvailabilityZones" text
  where
    params =
        [ "AvailabilityZones.member" |.#= zones
        , "LoadBalancerName" |= lb
        ]

disableAvailabilityZonesForLoadBalancer
    :: (MonadBaseControl IO m, MonadResource m)
    => [Text] -- ^ A list of Availability Zones to be removed from the LoadBalancer.
    -> Text -- ^ The name associated with the LoadBalancer.
    -> ELB m [Text] -- ^ A list of updated Availability Zones for the LoadBalancer.
disableAvailabilityZonesForLoadBalancer zones lb =
    elbQuery "DisableAvailabilityZonesForLoadBalancer" params $ members "AvailabilityZones" text
  where
    params =
        [ "AvailabilityZones.member" |.#= zones
        , "LoadBalancerName" |= lb
        ]

createLBCookieStickinessPolicy
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Int -- ^ The time period in seconds after which the cookie should be considered stale. Not specifying this parameter indicates that the sticky session will last for the duration of the browser session.
    -> Text -- ^ The name associated with the LoadBalancer.
    -> Text -- ^ The name of the policy being created.
    -> ELB m ()
createLBCookieStickinessPolicy period lb policy =
    elbQuery "CreateLBCookieStickinessPolicy" params $ getT_ "CreateLBCookieStickinessPolicyResult"
  where
    params =
        [ "CookieExpirationPeriod" |=? toText <$> period
        , "LoadBalancerName" |= lb
        , "PolicyName" |= policy
        ]

createAppCookieStickinessPolicy
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ Name of the application cookie used for stickiness.
    -> Text -- ^ The name associated with the LoadBalancer.
    -> Text -- ^ The name of the policy being created.
    -> ELB m ()
createAppCookieStickinessPolicy cookieName lb policy =
    elbQuery "CreateAppCookieStickinessPolicy" params $ getT_ "CreateAppCookieStickinessPolicyResult"
  where
    params =
        [ "CookieName" |= cookieName
        , "LoadBalancerName" |= lb
        , "PolicyName" |= policy
        ]

setLoadBalancerPoliciesOfListener
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^  The name associated with the LoadBalancer.
    -> Int -- ^ The external port of the LoadBalancer with which this policy applies to.
    -> [Text] -- ^ List of policies to be associated with the listener.
    -> ELB m ()
setLoadBalancerPoliciesOfListener lb port policies =
    elbQuery "SetLoadBalancerPoliciesOfListener" params $ getT_ "SetLoadBalancerPoliciesOfListenerResult"
  where
    params =
        [ "LoadBalancerName" |= lb
        , "LoadBalancerPort" |= toText port
        , if null policies then "PolicyNames" |= "" else "PolicyNames.member" |.#= policies
        ]

setLoadBalancerPoliciesForBackendServer
    :: (MonadBaseControl IO m, MonadResource m)
    => Int -- ^ The port number associated with the back-end server.
    -> Text -- ^ The mnemonic name associated with the LoadBalancer.
    -> [Text] -- ^ List of policy names to be set.
    -> ELB m ()
setLoadBalancerPoliciesForBackendServer port lb policies =
    elbQuery "SetLoadBalancerPoliciesForBackendServer" params $ getT_ "SetLoadBalancerPoliciesForBackendServerResult"
  where
    params =
        [ "InstancePort" |= toText port
        , "LoadBalancerName" |= lb
        , "PolicyNames.member" |.#= policies
        ]
