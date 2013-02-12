{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import AWSTests.EC2Tests
import AWSTests.RDSTests
import AWSTests.ELBTests
import AWSTests.CloudWatchTests

main :: IO ()
main = do
    -- EC2 Tests
    runInstanceTests
    runVpcTests
    runAddressTests
    runSnapshotTests
    runImageTests
    runSubnetsTests
    runVolumeTests
    runSecurityGroupTests
    runRouteTests
    runRouteTableTests
    runRegionTests
    runInstanceTests
    runAvailabilityZoneTests
    runTagTests
    runKeyPairTests
    runNetworkInterfaceTests
    runNetworkInterfaceAttributeTests
    runPlacementGroupTests
    runConversionTaskTests

    -- RDS Tests
    runDBInstanceTests
    runDBParameterGroupTests
    runDBSecurityGroupTests
    runDBSnapshotTests
    runDBSubnetGroupTests
    runEventTests

    -- ELB Tests
    runLoadBalancerTests

    -- CloudWatch Tests
    runAlarmTests
    runMetricTests
