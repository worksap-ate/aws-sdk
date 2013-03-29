module Main where

import EC2Tests.InstanceTests
import EC2Tests.AddressTests
import EC2Tests.SnapshotTests
import EC2Tests.ImageTests
import EC2Tests.SubnetsTests
import EC2Tests.VolumeTests
import EC2Tests.SecurityGroupTests
import EC2Tests.RouteTests
import EC2Tests.RouteTableTests
import EC2Tests.RegionTests
import EC2Tests.AvailabilityZoneTests
import EC2Tests.TagTests
import EC2Tests.KeyPairTests
import EC2Tests.NetworkInterfaceTests
import EC2Tests.NetworkInterfaceAttributeTests
import EC2Tests.PlacementGroupTests
import EC2Tests.ConversionTaskTests
import EC2Tests.AclTests

main :: IO ()
main = do
    runInstanceTests
    runAddressTests
    runSnapshotTests
    runImageTests
    runSubnetsTests
    runVolumeTests
    runSecurityGroupTests
    runRouteTests
    runRouteTableTests
    runRegionTests
    runAvailabilityZoneTests
    runTagTests
    runKeyPairTests
    runNetworkInterfaceTests
    runNetworkInterfaceAttributeTests
    runPlacementGroupTests
    runConversionTaskTests
    runAclTests
