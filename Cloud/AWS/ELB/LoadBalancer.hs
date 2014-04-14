{-# LANGUAGE FlexibleContexts, RankNTypes, RecordWildCards #-}

module Cloud.AWS.ELB.LoadBalancer
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

import Data.Text (Text, empty)
import Control.Applicative hiding (empty)
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)
import Cloud.AWS.Lib.Parser.Unordered (XmlElement, (.<), content, element, elementM)

import Cloud.AWS.Lib.Parser (members)
import Cloud.AWS.Lib.Query

import Cloud.AWS.ELB.Types
import Cloud.AWS.ELB.Internal

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

sinkLoadBalancers :: (MonadThrow m, Applicative m)
    => XmlElement -> m [LoadBalancer]
sinkLoadBalancers = members "LoadBalancerDescriptions" $ \xml ->
    LoadBalancer
    <$> members "SecurityGroups" content xml
    <*> xml .< "CreatedTime"
    <*> xml .< "LoadBalancerName"
    <*> element "HealthCheck" sinkHealthCheck xml
    <*> xml .< "VPCId"
    <*> members "ListenerDescriptions" lisDescConv xml
    <*> members "Instances" sinkInstance xml
    <*> element "Policies" polConv xml
    <*> members "AvailabilityZones" content xml
    <*> xml .< "CanonicalHostedZoneName"
    <*> xml .< "CanonicalHostedZoneNameID"
    <*> xml .< "Scheme"
    <*> elementM "SourceSecurityGroup" secConv xml
    <*> xml .< "DNSName"
    <*> members "BackendServerDescriptions" srvConv xml
    <*> members "Subnets" content xml
  where
    lisDescConv e = ListenerDescription
        <$> members "PolicyNames" content e
        <*> element "Listener" lisConv e
    lisConv e = Listener
        <$> e .< "Protocol"
        <*> e .< "LoadBalancerPort"
        <*> e .< "InstanceProtocol"
        <*> e .< "SSLCertificateId"
        <*> e .< "InstancePort"
    polConv e = Policies
        <$> members "AppCookieStickinessPolicies" appCookieConv e
        <*> members "OtherPolicies" content e
        <*> members "LBCookieStickinessPolicies" lbCookieConv e
    appCookieConv e = AppCookieStickinessPolicy
        <$> e .< "CookieName"
        <*> e .< "PolicyName"
    lbCookieConv e = LBCookieStickinessPolicy
        <$> e .< "PolicyName"
        <*> e .< "CookieExpirationPeriod"
    secConv e = SourceSecurityGroup
        <$> e .< "OwnerAlias"
        <*> e .< "GroupName"
    srvConv e = BackendServerDescription
        <$> e .< "InstancePort"
        <*> members "PolicyNames" content e

sinkInstance :: (MonadThrow m, Applicative m)
    => XmlElement -> m Instance
sinkInstance xml = Instance <$> xml .< "InstanceId"

sinkHealthCheck :: (MonadThrow m, Applicative m)
    => XmlElement -> m HealthCheck
sinkHealthCheck xml =
    HealthCheck
    <$> xml .< "Interval"
    <*> xml .< "Target"
    <*> xml .< "HealthyThreshold"
    <*> xml .< "Timeout"
    <*> xml .< "UnhealthyThreshold"

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
    elbQuery "CreateLoadBalancer" params (.< "DNSName")
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
            , "LoadBalancerPort" |= lbport
            , "InstanceProtocol" |= iprot
            , "SSLCertificateId" |=? cert
            , "InstancePort" |= iport
            ]

deleteLoadBalancer
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ LoadBalancerName
    -> ELB m ()
deleteLoadBalancer name = elbQuery "DeleteLoadBalancer" params (.< "DeleteLoadBalancerResult")
  where
    params = ["LoadBalancerName" |= name]

attachLoadBalancerToSubnets
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The name associated with the LoadBalancer.
    -> [Text] -- ^ A list of subnet IDs to add for the LoadBalancer.
    -> ELB m [Text] -- ^ A list of subnet IDs added for the LoadBalancer.
attachLoadBalancerToSubnets name subnets =
    elbQuery "AttachLoadBalancerToSubnets" params $ members "Subnets" content
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
    elbQuery "DetachLoadBalancerFromSubnets" params $ members "Subnets" content
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
    elbQuery "ApplySecurityGroupsToLoadBalancer" params $
        members "SecurityGroups" content
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
    elbQuery "RegisterInstancesWithLoadBalancer" params $
        members "Instances" sinkInstance
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
    elbQuery "DeregisterInstancesFromLoadBalancer" params $
        members "Instances" sinkInstance
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
    elbQuery "SetLoadBalancerListenerSSLCertificate" params
    $ (.< "SetLoadBalancerListenerSSLCertificateResult")
  where
    params =
        [ "LoadBalancerName" |= lb
        , "LoadBalancerPort" |= port
        , "SSLCertificateId" |= cert
        ]

createLoadBalancerListeners
    :: (MonadBaseControl IO m, MonadResource m)
    => [Listener] -- ^ A list of Listeners
    -> Text -- ^ The name of the LoadBalancer.
    -> ELB m ()
createLoadBalancerListeners listeners lb =
    elbQuery "CreateLoadBalancerListeners" params $
        (.< "CreateLoadBalancerListenersResult")
  where
    params =
        [ "Listeners.member" |.#. map toListenerParam listeners
        , "LoadBalancerName" |= lb
        ]

toListenerParam :: Listener -> [QueryParam]
toListenerParam Listener{..} =
    [ "Protocol" |= listenerProtocol
    , "LoadBalancerPort" |= listenerLoadBalancerPort
    , "InstanceProtocol" |= listenerInstanceProtocol
    , "SSLCertificateId" |=? listenerSSLCertificateId
    , "InstancePort" |= listenerInstancePort
    ]

deleteLoadBalancerListeners
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The mnemonic name associated with the LoadBalancer.
    -> [Int] -- ^ The client port number(s) of the LoadBalancerListener(s) to be removed.
    -> ELB m ()
deleteLoadBalancerListeners lb ports =
    elbQuery "DeleteLoadBalancerListeners" params $
        (.< "DeleteLoadBalancerListenersResult")
  where
    params =
        [ "LoadBalancerName" |= lb
        , "LoadBalancerPorts.member" |.#= ports
        ]

describeLoadBalancerPolicies
    :: (MonadBaseControl IO m, MonadResource m)
    => Maybe Text -- ^ The mnemonic name associated with the LoadBalancer.
    -> [Text] -- ^ The names of LoadBalancer policies you've created or Elastic Load Balancing sample policy names.
    -> ELB m [PolicyDescription]
describeLoadBalancerPolicies mlb policies =
    elbQuery "DescribeLoadBalancerPolicies" params $
        members "PolicyDescriptions" sinkPolicyDescription
  where
    params =
        [ "LoadBalancerName" |=? mlb
        , "PolicyNames.member" |.#= policies
        ]

sinkPolicyDescription :: (MonadThrow m, Applicative m)
    => XmlElement -> m PolicyDescription
sinkPolicyDescription xml =
    PolicyDescription
    <$> xml .< "PolicyName"
    <*> xml .< "PolicyTypeName"
    <*> members "PolicyAttributeDescriptions" sinkPolicyAttribute xml

sinkPolicyAttribute :: (MonadThrow m, Applicative m)
    => XmlElement -> m PolicyAttribute
sinkPolicyAttribute xml =
    PolicyAttribute
    <$> xml .< "AttributeName"
    <*> xml .< "AttributeValue"

describeLoadBalancerPolicyTypes
    :: (MonadBaseControl IO m, MonadResource m)
    => [Text] -- ^ Specifies the name of the policy types.
    -> ELB m [PolicyType]
describeLoadBalancerPolicyTypes typeNames =
    elbQuery "DescribeLoadBalancerPolicyTypes" params
    $ members "PolicyTypeDescriptions" sinkPolicyType
  where
    params = ["PolicyTypeNames.member" |.#= typeNames]

sinkPolicyType :: (MonadThrow m, Applicative m)
    => XmlElement -> m PolicyType
sinkPolicyType xml =
    PolicyType
    <$> members "PolicyAttributeTypeDescriptions" sinkPolicyAttributeType xml
    <*> xml .< "PolicyTypeName"
    <*> xml .< "Description"

sinkPolicyAttributeType :: (MonadThrow m, Applicative m)
    => XmlElement -> m PolicyAttributeType
sinkPolicyAttributeType xml =
    PolicyAttributeType
    <$> xml .< "AttributeName"
    <*> xml .< "AttributeType"
    <*> xml .< "DefaultValue"
    <*> xml .< "Cardinality"
    <*> xml .< "Description"

createLoadBalancerPolicy
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ The name associated with the LoadBalancer for which the policy is being created.
    -> [PolicyAttribute] -- ^ A list of attributes associated with the policy being created.
    -> Text -- ^ The name of the LoadBalancer policy being created.
    -> Text -- ^ The name of the base policy type being used to create this policy.
    -> ELB m ()
createLoadBalancerPolicy lb attrs name typeName =
    elbQuery "CreateLoadBalancerPolicy" params $
        (.< "CreateLoadBalancerPolicyResult")
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
    elbQuery "DeleteLoadBalancerPolicy" params $
        (.< "DeleteLoadBalancerPolicyResult")
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
    elbQuery "DescribeInstanceHealth" params $
        members "InstanceStates" sinkInstanceState
  where
    params =
        [ "Instances.member" |.#. map toInstanceParam insts
        , "LoadBalancerName" |= lb
        ]

sinkInstanceState :: (MonadThrow m, Applicative m)
    => XmlElement -> m InstanceState
sinkInstanceState xml =
    InstanceState
    <$> xml .< "Description"
    <*> xml .< "InstanceId"
    <*> xml .< "State"
    <*> xml .< "ReasonCode"

configureHealthCheck
    :: (MonadBaseControl IO m, MonadResource m)
    => HealthCheck -- ^ A structure containing the configuration information for the new healthcheck.
    -> Text -- ^ The mnemonic name associated with the LoadBalancer.
    -> ELB m HealthCheck
configureHealthCheck hc lb =
    elbQuery "ConfigureHealthCheck" params $
        element "HealthCheck" sinkHealthCheck
  where
    params =
        [ "HealthCheck" |. toHealthCheckParams hc
        , "LoadBalancerName" |= lb
        ]

toHealthCheckParams :: HealthCheck -> [QueryParam]
toHealthCheckParams HealthCheck{..} =
    [ "HealthyThreshold" |= healthCheckHealthyThreshold
    , "Interval" |= healthCheckInterval
    , "Target" |= healthCheckTarget
    , "Timeout" |= healthCheckTimeout
    , "UnhealthyThreshold" |= healthCheckUnhealthyThreshold
    ]

enableAvailabilityZonesForLoadBalancer
    :: (MonadBaseControl IO m, MonadResource m)
    => [Text] -- ^ A list of new Availability Zones for the LoadBalancer.
    -> Text -- ^ The name associated with the LoadBalancer.
    -> ELB m [Text] -- ^ An updated list of Availability Zones for the LoadBalancer.
enableAvailabilityZonesForLoadBalancer zones lb =
    elbQuery "EnableAvailabilityZonesForLoadBalancer" params
    $ members "AvailabilityZones" content
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
    elbQuery "DisableAvailabilityZonesForLoadBalancer" params
    $ members "AvailabilityZones" content
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
    elbQuery "CreateLBCookieStickinessPolicy" params (.< "CreateLBCookieStickinessPolicyResult")
  where
    params =
        [ "CookieExpirationPeriod" |=? period
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
    elbQuery "CreateAppCookieStickinessPolicy" params (.< "CreateAppCookieStickinessPolicyResult")
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
    elbQuery "SetLoadBalancerPoliciesOfListener" params (.< "SetLoadBalancerPoliciesOfListenerResult")
  where
    params =
        [ "LoadBalancerName" |= lb
        , "LoadBalancerPort" |= port
        , if null policies then "PolicyNames" |= empty else "PolicyNames.member" |.#= policies
        ]

setLoadBalancerPoliciesForBackendServer
    :: (MonadBaseControl IO m, MonadResource m)
    => Int -- ^ The port number associated with the back-end server.
    -> Text -- ^ The mnemonic name associated with the LoadBalancer.
    -> [Text] -- ^ List of policy names to be set.
    -> ELB m ()
setLoadBalancerPoliciesForBackendServer port lb policies =
    elbQuery "SetLoadBalancerPoliciesForBackendServer" params (.< "SetLoadBalancerPoliciesForBackendServerResult")
  where
    params =
        [ "InstancePort" |= port
        , "LoadBalancerName" |= lb
        , "PolicyNames.member" |.#= policies
        ]
