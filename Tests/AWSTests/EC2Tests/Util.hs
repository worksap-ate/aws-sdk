{-# LANGUAGE FlexibleContexts #-}
module AWSTests.EC2Tests.Util
    ( testEC2
    , testEC2'
    , sleep
    , withVpc
    , withSubnet
    , withInstance
    )
    where

import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Class (lift)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Applicative
import qualified Control.Exception.Lifted as E
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)

import AWS
import AWS.EC2
import AWS.EC2.Types

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

sleep :: MonadIO m => Int -> EC2 m ()
sleep sec = liftIO $ threadDelay $ sec * 1000 * 1000

withVpc
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ CIDR
    -> (Vpc -> EC2 m a)
    -> EC2 m a
withVpc cidr = E.bracket
    (createVpc cidr Nothing <* sleep 2)
    (\vpc -> deleteVpc $ vpcId vpc)

withSubnet
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -- ^ CIDR
    -> (Subnet -> EC2 m a)
    -> EC2 m a
withSubnet cidr f = withVpc cidr $ \vpc -> E.bracket
    (createSubnet (CreateSubnetRequest (vpcId vpc) (vpcCidrBlock vpc) Nothing) <* sleep 2)
    (\subnet -> deleteSubnet $ subnetId subnet)
    f

withInstance
    :: (MonadBaseControl IO m, MonadResource m)
    => RunInstancesRequest
    -> (Instance -> EC2 m a)
    -> EC2 m a
withInstance req = E.bracket
    (head . reservationInstanceSet <$> runInstances req <* sleep 2)
    (\i -> terminateInstances [instanceId i])
