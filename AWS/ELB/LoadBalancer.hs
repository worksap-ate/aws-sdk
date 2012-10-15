{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.ELB.LoadBalancer
    ( describeLoadBalancers
    ) where

import Data.Text (Text)
import Data.Conduit
import Control.Applicative
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.XML.Types (Event(..))

import AWS.Util
import AWS.Lib.Parser
import AWS.Lib.Query

import AWS.ELB.Types
import AWS.ELB.Internal

describeLoadBalancers
    :: (MonadBaseControl IO m, MonadResource m)
    => [Text] -- ^ LoadBalancerNames
    -> Maybe Text -- ^ Marker
    -> ELB m [LoadBalancerDescription]
describeLoadBalancers lbs marker =
    elbQuery "DescribeLoadBalancers" params sinkLoadBalancers
  where
    params = [ArrayParams "LoadBalancerNames.member" lbs]
        ++ maybe [] (\a -> [ValueParam "Marker" a]) marker

sinkLoadBalancers :: MonadThrow m
    => GLSink Event m [LoadBalancerDescription]
sinkLoadBalancers = members "LoadBalancerDescriptions" $
    LoadBalancerDescription
    <$> members "SecurityGroups" text
    <*> getT "LoadBalancerName"
    <*> getF "CreatedTime" textToTime
    <*> element "HealthCheck"
        (HealthCheck
        <$> getF "Interval" textToInt
        <*> getT "Target"
        <*> getF "HealthyThreshold" textToInt
        <*> getF "Timeout" textToInt
        <*> getF "UnhealthyThreshold" textToInt
        )
    <*> getMT "VPCId"
    <*> members "ListenerDescriptions"
        (ListenerDescription
        <$> members "PolicyNames" text
        <*> element "Listener"
            (Listener
            <$> getT "Protocol"
            <*> getF "LoadBalancerPort" textToInt
            <*> getT "InstanceProtocol"
            <*> getMT "SSLCertificateId"
            <*> getF "InstancePort" textToInt
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
            <$> getF "CookieExpirationPeriod" textToInt
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
        <$> getF "InstancePort" textToInt
        <*> members "PolicyNames" text
        )
    <*> members "Subnets" text
