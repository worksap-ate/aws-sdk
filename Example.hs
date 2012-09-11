{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Example where

import Data.ByteString.Char8 ()
import qualified Data.ByteString.Char8 as BSC
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Network.HTTP.Conduit as H
import Control.Monad.IO.Class (liftIO)
import Data.Time

import AWS
import AWS.Credential
import AWS.EC2

paramsRegions :: [QueryParam]
paramsRegions =
    [ ("Action", "DescribeRegions")
    , ("Version", "2012-07-20")
    , ("SignatureVersion", "2")
    , ("SignatureMethod", "HmacSHA256")
    ]

paramsZones :: [QueryParam]
paramsZones =
    [ ("Action", "DescribeAvailabilityZones")
    , ("Version", "2012-07-20")
    , ("SignatureVersion", "2")
    , ("SignatureMethod", "HmacSHA256")
    ]

main :: IO ()
main = do
    cred <- loadCredential
    time <- getCurrentTime
    let url = mkUrl UsEast1 cred time
--    let url = mkUrl' UsEast1 cred time paramsRegions
--    let url = mkUrl' UsEast1 cred time paramsZones
    doc <- runResourceT $ do
        manager <- liftIO $ H.newManager H.def
        request <- liftIO $ H.parseUrl (BSC.unpack url)
        response <- describeImages request manager
--        response <- describeRegions request manager
--        response <- describeAvailabilityZones request manager
        responseBody response $$ CL.consume
    print doc
    putStr "Length: "
    print $ length doc

