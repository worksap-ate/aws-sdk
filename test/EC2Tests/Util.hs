{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module EC2Tests.Util
    ( testEC2
    , testEC2'
    , withVpc
    , withSubnet
    , withSubnet'
    , withInstance
    , testRunInstancesRequest
    , waitForInstanceState
    , withRouteTable
    , withInternetGateway
    , withInternetGatewayAttached
    , withNetworkAcl
    , withNetworkAclEntry
    , withNetworkInterface
    , waitForNetworkInterfaceStatus
    , withSecurityGroup
    , withSnapshot
    , withVolume
    , withCustomerGateway
    , withVpnGateway
    , withVpnGatewayAttached
    , withVpnConnection
    , withAddress
    , withDhcpOptions
    , withImage
    , waitForImageState
    )
    where

import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (ResourceT, runResourceT, MonadResource, MonadBaseControl)
import Control.Applicative
import qualified Control.Exception.Lifted as E
import Data.Text (Text)
import Data.IP (IPv4, AddrRange)

import Cloud.AWS.EC2
import Cloud.AWS.EC2.Types
import qualified Cloud.AWS.EC2.Util as Util

testEC2
    :: Text
    -> EC2 (ResourceT IO) (ResumableSource (ResourceT IO) a)
    -> IO [a]
testEC2 region request = do
    runResourceT $ runEC2 $ do
        setRegion region
        response <- request
        lift $ response $$+- CL.consume

testEC2'
    :: Text
    -> EC2 (ResourceT IO) a
    -> IO a
testEC2' region request = do
    runResourceT $ runEC2 $ do
        setRegion region
        request

withVpc
    :: (MonadBaseControl IO m, MonadResource m)
    => AddrRange IPv4 -- ^ CIDR
    -> (Vpc -> EC2 m a)
    -> EC2 m a
withVpc cidr = E.bracket
    (createVpc cidr Nothing <* Util.sleep 2)
    (\vpc -> deleteVpc $ vpcId vpc)

withSubnet
    :: (MonadBaseControl IO m, MonadResource m)
    => AddrRange IPv4 -- ^ CIDR
    -> (Subnet -> EC2 m a)
    -> EC2 m a
withSubnet cidr f = withVpc cidr $ \Vpc{vpcId = vpc} -> withSubnet' vpc cidr Nothing f

withSubnet'
    :: (MonadBaseControl IO m, MonadResource m)
    => Text
    -> AddrRange IPv4 -- ^ CIDR
    -> Maybe Text
    -> (Subnet -> EC2 m a)
    -> EC2 m a
withSubnet' vpc cidr zone = E.bracket
    (createSubnet (CreateSubnetRequest vpc cidr zone) <* Util.sleep 2)
    (Util.retry 5 10 . deleteSubnet . subnetId)

withInstance
    :: (MonadBaseControl IO m, MonadResource m)
    => RunInstancesRequest
    -> (Instance -> EC2 m a)
    -> EC2 m a
withInstance req = E.bracket
    (head . reservationInstanceSet <$> runInstances req <* Util.sleep 2)
    (\i -> terminateInstances [instanceId i])

testRunInstancesRequest :: RunInstancesRequest
testRunInstancesRequest = (defaultRunInstancesRequest "ami-087acb09" 1 1)
    { runInstancesRequestInstanceType = Just "t1.micro"
    }

waitForInstanceState :: (MonadBaseControl IO m, MonadResource m) => InstanceState -> Text -> EC2 m Reservation
waitForInstanceState s = Util.wait p desc
  where
    p r = (instanceState . head . reservationInstanceSet) r == s
    desc inst = Util.list $ describeInstances [inst] []

withRouteTable :: (MonadBaseControl IO m, MonadResource m) => Text -> (RouteTable -> EC2 m a) -> EC2 m a
withRouteTable vpc = E.bracket
    (createRouteTable vpc)
    (deleteRouteTable . routeTableId)

withInternetGateway :: (MonadBaseControl IO m, MonadResource m) => (InternetGateway -> EC2 m a) -> EC2 m a
withInternetGateway = E.bracket
    createInternetGateway
    (deleteInternetGateway . internetGatewayInternetGatewayId)

withInternetGatewayAttached :: (MonadBaseControl IO m, MonadResource m) => Text -> Text -> EC2 m a -> EC2 m a
withInternetGatewayAttached gateway vpc f = E.bracket
    (attachInternetGateway gateway vpc)
    (const $ detachInternetGateway gateway vpc)
    (const f)

withNetworkAcl :: (MonadBaseControl IO m, MonadResource m) => Text -> (NetworkAcl -> EC2 m a) -> EC2 m a
withNetworkAcl vpc = E.bracket (createNetworkAcl vpc) (deleteNetworkAcl . networkAclId)

withNetworkAclEntry :: (MonadBaseControl IO m, MonadResource m) => NetworkAclEntryRequest -> EC2 m a -> EC2 m a
withNetworkAclEntry req f = E.bracket
    (createNetworkAclEntry req)
    (const $ deleteNetworkAclEntry (networkAclEntryRequestNetworkAclId req) (networkAclEntryRequestRuleNumber req) (networkAclEntryRequestEgress req))
    (const f)

withNetworkInterface :: (MonadBaseControl IO m, MonadResource m) => Text -> (NetworkInterface -> EC2 m a) -> EC2 m a
withNetworkInterface subnet = E.bracket
    (createNetworkInterface subnet SecondaryPrivateIpAddressParamNothing Nothing [])
    (deleteNetworkInterface . networkInterfaceId)

waitForNetworkInterfaceStatus :: (MonadBaseControl IO m, MonadResource m) => NetworkInterfaceStatus -> Text -> EC2 m NetworkInterface
waitForNetworkInterfaceStatus s = Util.wait p desc
  where
    p r = networkInterfaceStatus r == s
    desc nic = Util.list $ describeNetworkInterfaces [nic] []

withSecurityGroup :: (MonadBaseControl IO m, MonadResource m) => Text -> Text -> Maybe Text -> (Maybe Text -> EC2 m a) -> EC2 m a
withSecurityGroup name desc mvpc = E.bracket
    (createSecurityGroup name desc mvpc)
    delete
  where
    delete Nothing = deleteSecurityGroup $ SecurityGroupRequestGroupName name
    delete (Just sg) = deleteSecurityGroup $ SecurityGroupRequestGroupId sg

withSnapshot :: (MonadBaseControl IO m, MonadResource m) => Text -> Maybe Text -> (Snapshot -> EC2 m a) -> EC2 m a
withSnapshot vol desc = E.bracket
    (createSnapshot vol desc)
    (deleteSnapshot . snapshotId)

withVolume :: (MonadBaseControl IO m, MonadResource m) => CreateVolumeRequest -> (Volume -> EC2 m a) -> EC2 m a
withVolume req = E.bracket
    (createVolume req)
    (deleteVolume . volumeId)

withCustomerGateway :: (MonadBaseControl IO m, MonadResource m) => Text -> IPv4 -> Int -> (CustomerGateway -> EC2 m a) -> EC2 m a
withCustomerGateway typ addr bgpasn = E.bracket
    (createCustomerGateway typ addr bgpasn)
    (Util.retry 5 30 . deleteCustomerGateway . customerGatewayId)

withVpnGateway :: (MonadBaseControl IO m, MonadResource m) => CreateVpnGatewayType -> Maybe Text -> (VpnGateway -> EC2 m a) -> EC2 m a
withVpnGateway typ zone = E.bracket
    (createVpnGateway typ zone)
    (Util.retry 5 30 . deleteVpnGateway . vpnGatewayId)

withVpnGatewayAttached :: (MonadBaseControl IO m, MonadResource m) => Text -> Text -> EC2 m a -> EC2 m a
withVpnGatewayAttached vgw vpc f = E.bracket
    (attachVpnGateway vgw vpc)
    (const $ detachVpnGateway vgw vpc)
    (const f)

withVpnConnection :: (MonadBaseControl IO m, MonadResource m) => Text -> Text -> Text -> Maybe Text -> Maybe Bool -> (VpnConnection -> EC2 m a) -> EC2 m a
withVpnConnection typ cgid vgid zone option = E.bracket
    (do
        vpn <- createVpnConnection typ cgid vgid zone option
        Util.wait
            (\connection -> vpnConnectionState connection == VpnConnectionStateAvailable)
            (\cid -> Util.list $ describeVpnConnections [cid] [])
            (vpnConnectionId vpn)
    )
    (deleteVpnConnection . vpnConnectionId)

withAddress :: (MonadBaseControl IO m, MonadResource m) => Bool -> (AllocateAddress -> EC2 m a) -> EC2 m a
withAddress isVpc = E.bracket
    (allocateAddress isVpc)
    release
  where
    release AllocateAddress{allocateAddressAllocationId = Just alloc} = releaseAddress Nothing (Just alloc)
    release AllocateAddress{allocateAddressPublicIp = ip} = releaseAddress (Just ip) Nothing

withDhcpOptions :: (MonadBaseControl IO m, MonadResource m) => [DhcpConfiguration] -> (DhcpOptions -> EC2 m a) -> EC2 m a
withDhcpOptions confs = E.bracket
    (createDhcpOptions confs)
    (deleteDhcpOptions . dhcpOptionsId)

withImage :: (MonadBaseControl IO m, MonadResource m) => Text -> Text -> Maybe Text -> Bool -> [BlockDeviceMappingParam] -> (Text -> EC2 m a) -> EC2 m a
withImage inst name desc noReboot bdms = E.bracket
    (createImage inst name desc noReboot bdms)
    deregisterImage

waitForImageState :: (MonadBaseControl IO m, MonadResource m) => ImageState -> Text -> EC2 m Image
waitForImageState s = Util.wait p desc
  where
    p Image{imageImageState = s'} = s' == s
    desc ami = Util.list $ describeImages [ami] [] [] []
