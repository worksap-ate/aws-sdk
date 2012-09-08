{-# LANGUAGE OverloadedStrings #-}
module AWS.Types where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Data.Map.Strict (Map)

class Endpoint a where
    endpointStr :: a -> ByteString

data Ec2Endpoint = UsEast1
                 | ApNortheast1

instance Endpoint Ec2Endpoint where
    endpointStr UsEast1      = "ec2.us-east-1.amazonaws.com"
    endpointStr ApNortheast1 = "ec2.ap-northeast-1.amazonaws.com"

type AccessKey = ByteString
type SecretAccessKey = ByteString
data Credential = Credential
    { accessKey :: AccessKey
    , secretAccessKey :: SecretAccessKey
    }
  deriving (Show)

type QueryParam = (ByteString, ByteString)
type QueryParams = Map ByteString ByteString
