{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import AWSTests.EC2Tests
import AWSTests.RDSTests
import AWSTests.ELBTests

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

    -- RDS Tests
    runDBInstanceTests
    runDBSnapshotTests

    -- ELB Tests
    runLoadBalancerTests
