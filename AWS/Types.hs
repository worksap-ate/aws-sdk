{-# LANGUAGE OverloadedStrings #-}
module AWS.Types
    ( Endpoint (..)
    , EC2Endpoint (..)
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()

class Endpoint a where
    endpointStr :: a -> ByteString

data EC2Endpoint = UsEast1
                 | ApNortheast1

instance Endpoint EC2Endpoint where
    endpointStr UsEast1      = "ec2.us-east-1.amazonaws.com"
    endpointStr ApNortheast1 = "ec2.ap-northeast-1.amazonaws.com"
