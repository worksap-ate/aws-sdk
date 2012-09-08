{-# LANGUAGE OverloadedStrings #-}
module Example where

import Data.ByteString.Char8 ()
import qualified Data.ByteString.Char8 as BSC
import Data.Conduit
import qualified Data.Conduit.List as CL
import Network.HTTP.Conduit (def, newManager)
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)

import AWS
import AWS.EC2

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
        manager <- liftIO $ newManager def
        response <- describeImages manager cred UsEast1 imageIds
        imagesSet response $$ CL.consume
    print doc
    putStr "Length: "
    print $ length doc
