{-# LANGUAGE OverloadedStrings #-}
module AWS where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.List as LIST
import           Data.Monoid
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.ByteString.Base64.Lazy as BASE
import qualified Network.HTTP.Types as HTTP

import System.IO (IOMode(..), withFile)
import System.Locale (defaultTimeLocale, iso8601DateFormat)
import Data.Time (UTCTime, formatTime)

import AWS.Util

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

loadCredential :: IO Credential
loadCredential = withFile (BSLC.unpack "aws.config") ReadMode $ \h -> do
    a <- BSC.hGetLine h
    s <- BSC.hGetLine h
    return $ Credential
        { accessKey = a
        , secretAccessKey = s
        }

type QueryParam = (ByteString, ByteString)

queryStr :: [QueryParam] -> ByteString
queryStr = concatWithSep "&" . map (concatParamWithSep "=")
  where
    concatParamWithSep :: ByteString -> QueryParam -> ByteString
    concatParamWithSep sep (k, v) = mconcat [k, sep, v]
    concatWithSep :: ByteString -> [ByteString] -> ByteString
    concatWithSep sep = foldl1 $ \a b -> mconcat [a, sep, b]

mkUrl' :: Endpoint end
        => end -> Credential -> UTCTime -> [QueryParam] -> ByteString
mkUrl' endpoint cred time params = url
  where
    t = awsTimeFormat time
    timeTuple = ("Timestamp", HTTP.urlEncode True t)
    aKeyTuple = ("AWSAccessKeyId", accessKey cred)
    paramTuples = timeTuple : aKeyTuple : params
    qparam = queryStr $ LIST.sort paramTuples
    sig = signature endpoint (secretAccessKey cred) qparam
    url = mconcat
        [ "https://"
        , endpointStr endpoint
        , "/?"
        , qparam
        , "&Signature="
        , sig
        ]

awsTimeFormat :: UTCTime -> ByteString
awsTimeFormat time = BSC.pack $ formatTime defaultTimeLocale (iso8601DateFormat $ Just "%XZ") time

signature :: Endpoint end 
          => end -> SecretAccessKey -> ByteString -> ByteString
signature endpoint secret query = urlstring
  where
    stringToSign = mconcat
        [ "GET\n"
        , endpointStr endpoint
        , "\n/\n"
        , query
        ]
    signedString = SHA.hmacSha256 (toL secret) (toL stringToSign)
    base64string = BASE.encode $ SHA.bytestringDigest signedString
    urlstring = HTTP.urlEncode True (toS base64string)
