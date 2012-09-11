{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Example where

import Data.ByteString.Char8 ()
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Network.HTTP.Conduit as HTTP
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)

import AWS.Credential
import AWS.Types
import AWS.EC2

imageIds :: [ByteString]
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

main :: IO ()
main = do
    cred <- loadCredential
    doc <- runResourceT $ do
        manager <- liftIO $ HTTP.newManager HTTP.def
--        response <- describeRegions manager cred UsEast1 []
--        response <- describeAvailabilityZones manager cred UsEast1 []
        response <- describeImages manager cred UsEast1 imageIds
        responseBody response $$ CL.consume
    print doc
    putStr "Length: "
    print $ length doc
