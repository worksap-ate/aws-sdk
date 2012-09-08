{-# LANGUAGE OverloadedStrings #-}
module AWS where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Monoid
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.ByteString.Base64.Lazy as BASE
import qualified Data.Map.Strict as Map
import qualified Network.HTTP.Types as HTTP

import System.IO (IOMode(..), withFile)
import System.Locale (defaultTimeLocale, iso8601DateFormat)
import Data.Time (UTCTime, formatTime)

import AWS.Types
import AWS.Util

loadCredential :: IO Credential
loadCredential = withFile (BSLC.unpack "aws.config") ReadMode $ \h -> do
    a <- BSC.hGetLine h
    s <- BSC.hGetLine h
    return $ Credential
        { accessKey = a
        , secretAccessKey = s
        }

queryStr :: QueryParams -> ByteString
queryStr = concatWithSep "&" . Map.foldlWithKey concatWithEqual []
  where
    concatWithEqual acc key val = acc ++ [key `mappend` "=" `mappend` val] -- FIXME not to use ++
    concatWithSep :: ByteString -> [ByteString] -> ByteString
    concatWithSep sep = foldl1 $ \a b -> mconcat [a, sep, b]

mkUrl' :: Endpoint end
        => end -> Credential -> UTCTime -> QueryParams -> ByteString
mkUrl' endpoint cred time params = url
  where
    t = awsTimeFormat time
    timeTuple = ("Timestamp", HTTP.urlEncode True t)
    aKeyTuple = ("AWSAccessKeyId", accessKey cred)
    paramTuples = Map.union params $ Map.fromList $ timeTuple : [aKeyTuple]
    qparam = queryStr paramTuples
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
