{-# LANGUAGE FlexibleContexts, RankNTypes, RecordWildCards #-}

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
    ) where

import Data.Text (Text)
import Data.IP (IPv4, AddrRange)
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
    => GLSink Event m [LoadBalancer]
sinkLoadBalancers = members "LoadBalancerDescriptions" $
    LoadBalancer
    <$> members "SecurityGroups" text
    <*> getT "CreatedTime"
    <*> getT "LoadBalancerName"
    <*> element "HealthCheck"
        (HealthCheck
        <$> getT "Interval"
        <*> getT "Target"
        <*> getT "HealthyThreshold"
        <*> getT "Timeout"
        <*> getT "UnhealthyThreshold"
        )
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
            <$> getT "CookieExpirationPeriod"
            <*> getT "PolicyName"
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

sinkInstance :: MonadThrow m => GLSink Event m Instance
sinkInstance = Instance <$> getT "InstanceId"

createLoadBalancer
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ LoadBalancerName
    -> [Listener] -- ^ Listeners
    -> [Text] -- ^ AvailabilityZones
    -> Maybe Text -- ^ Scheme
    -> [Text] -- ^ SecurityGroups
    -> [AddrRange IPv4] -- ^ Subnets
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
        , "Subnets.member" |.#= map toText subnets
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

sinkPolicyDescription :: MonadThrow m => GLSink Event m PolicyDescription
sinkPolicyDescription =
    PolicyDescription
    <$> getT "PolicyName"
    <*> getT "PolicyTypeName"
    <*> members "PolicyAttributeDescriptions" sinkPolicyAttribute

sinkPolicyAttribute :: MonadThrow m => GLSink Event m PolicyAttribute
sinkPolicyAttribute =
    PolicyAttribute
    <$> getT "AttributeName"
    <*> getT "AttributeValue"
