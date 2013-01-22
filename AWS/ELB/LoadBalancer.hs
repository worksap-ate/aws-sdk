{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.ELB.LoadBalancer
    ( describeLoadBalancers
    , createLoadBalancer
    , deleteLoadBalancer
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
    <*> members "Instances"
        (Instance <$> getT "InstanceId")
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
