{-# LANGUAGE OverloadedStrings #-}
module AWS where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.List as LIST
import           Data.Monoid
import qualified Data.Digest.Pure.SHA as SHA
import qualified Codec.Binary.Base64 as Base64
import qualified Codec.Binary.Url as Url

import System.IO (IOMode(..), withFile)
import System.Locale (defaultTimeLocale, iso8601DateFormat)
import Data.Time (UTCTime, formatTime)
import AWS.EC2

class Endpoint a where
    endpointStr :: a -> ByteString

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

loadCredential :: IO Credential
loadCredential = withFile (LC.unpack "aws.config") ReadMode $ \h -> do
    a <- BC.hGetLine h
    s <- BC.hGetLine h
    return $ Credential
        { accessKey = LC.pack $ BC.unpack a
        , secretAccessKey = LC.pack $ BC.unpack s
        }

mkUrl :: Endpoint end
      => end -> Credential -> UTCTime -> String
mkUrl end cred time = mkUrl' end qparam sig
  where
    t = awsTimeFormat time
    qparam = queryStr $ LIST.sort $ params (accessKey cred) (LC.pack t)
    sig = signature end (secretAccessKey cred) qparam

mkUrl' :: Endpoint end => end -> ByteString -> ByteString -> String
mkUrl' endpoint qparam sig =
    LC.unpack $ mconcat
    [ "https://"
    , endpointStr endpoint
    , "/?"
    , qparam
    , "&Signature="
    , sig
    ]

awsTimeFormat :: UTCTime -> String
awsTimeFormat time = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%XZ") time

signature :: Endpoint end 
          => end -> SecretAccessKey -> ByteString -> ByteString
signature endpoint secret query =
    wrap Url.encode base64string
  where
    stringToSign = mconcat
        [ "GET\n"
        , endpointStr endpoint
        , "\n/\n"
        , query
        ]
    signedString = SHA.hmacSha256 secret stringToSign
    base64string = wrap Base64.encode $ SHA.bytestringDigest signedString

