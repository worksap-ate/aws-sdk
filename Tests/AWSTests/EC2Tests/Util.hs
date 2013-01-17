{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module AWSTests.EC2Tests.Util
    ( testEC2
    , testEC2'
    , withVpc
    , withSubnet
    , withInstance
    )
    where

import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Class (lift)
import Control.Applicative
import qualified Control.Exception.Lifted as E
import Data.Text (Text)
import Data.IP (IPv4, AddrRange)

import AWS
import AWS.EC2
import AWS.EC2.Types
import AWS.EC2.Util (retry, sleep)

testEC2
    :: Text
    -> EC2 (ResourceT IO) (ResumableSource (ResourceT IO) a)
    -> IO [a]
testEC2 region request = do
    cred <- loadCredential
    runResourceT $ do
        runEC2 cred $ do
            setRegion region
            response <- request
            lift $ response $$+- CL.consume

testEC2'
    :: Text
    -> EC2 (ResourceT IO) a
    -> IO a
testEC2' region request = do
    cred <- loadCredential
    runResourceT $ do
        runEC2 cred $ do
            setRegion region
            request

withVpc
    :: (MonadBaseControl IO m, MonadResource m)
    => AddrRange IPv4 -- ^ CIDR
    -> (Vpc -> EC2 m a)
    -> EC2 m a
withVpc cidr = E.bracket
    (createVpc cidr Nothing <* sleep 2)
    (\vpc -> deleteVpc $ vpcId vpc)

withSubnet
    :: (MonadBaseControl IO m, MonadResource m)
    => AddrRange IPv4 -- ^ CIDR
    -> (Subnet -> EC2 m a)
    -> EC2 m a
withSubnet cidr f = withVpc cidr $ \vpc -> E.bracket
    (createSubnet (CreateSubnetRequest (vpcId vpc) cidr Nothing) <* sleep 2)
    (\subnet -> retry 5 10 $ deleteSubnet $ subnetId subnet)
    f

withInstance
    :: (MonadBaseControl IO m, MonadResource m)
    => RunInstancesRequest
    -> (Instance -> EC2 m a)
    -> EC2 m a
withInstance req = E.bracket
    (head . reservationInstanceSet <$> runInstances req <* sleep 2)
    (\i -> terminateInstances [instanceId i])
