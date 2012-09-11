{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Example where

import Data.ByteString.Char8 ()
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Network.HTTP.Conduit as HTTP
import Control.Monad.IO.Class (liftIO)

import AWS.Credential
import AWS.Types
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
    let imageIds =
            [ "ami-e565ba8c"
            , "ami-6678da0f"
            , "ami-0067ca69"
            , "ami-f21aff9b"
            , "ami-03d37c6a"
            , "aki-f5c1219c"
            , "ari-f606f39f"
            , "ami-8a3cc9e3"
            ]
    cred <- loadCredential
    doc <- runResourceT $ do
        manager <- liftIO $ HTTP.newManager HTTP.def
        response <- describeImages manager cred UsEast1 imageIds
        responseBody response $$ CL.consume
    print doc
    putStr "Length: "
    print $ length doc
