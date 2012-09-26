{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Example where

import Data.ByteString.Char8 ()
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Control.Monad.Trans.Control
import qualified Data.ByteString.Base64 as Base64

import AWS
import AWS.EC2
import AWS.EC2.Types
import AWS.Util

imageIds :: [Text]
imageIds =
    [ "ami-e565ba8c"
    , "ami-6678da0f"
    , "ami-0067ca69"
    , "ami-f21aff9b"
    , "ami-03d37c6a"
    , "aki-f5c1219c"
    , "ari-f606f39f"
    , "ami-8a3cc9e3"
    ]

addressTest :: (MonadResource m, MonadBaseControl IO m)
    => EC2 m [Address]
addressTest = do
    addr <- allocateAddress False
    ret <- releaseAddress (Just $ alaPublicIp addr) Nothing
    liftIO $ print ret

    addr2 <- allocateAddress True
    ret2 <- releaseAddress Nothing (alaAllocationId addr2)
    liftIO $ print ret2

    res <- describeAddresses [] [] []
    lift $ res $$ CL.consume

main :: IO ()
main = do
    cred <- loadCredential
    doc <- runResourceT $ do
        ctx <- liftIO $ newEC2Context cred
        runEC2 ctx $ do
--            addressTest
--            setEndpoint ApNortheast1
--            response <- describeAvailabilityZones [] []
--            response <- describeRegions [] []
--            response <- describeImages imageIds [] [] []
--            response <- describeImages [] [] [] []
--            response <- describeInstances [] []
--            response <- describeInstanceStatus [] True [] Nothing
            response <- describeTags []
--            lift $ response $$ CL.consume
--            runInstances $ defaultRunInstancesParam "ami-31814f58" 1 1
--            getConsoleOutput "i-01c8226d"
    print doc
--    putStr "Length: "
--    print $ length doc
