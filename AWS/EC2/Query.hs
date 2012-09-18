{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Query
    ( EC2Context(..)
    , EC2
    , ec2Query
    , QueryParams(..)
    , Filter
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Char8 as BSC

import Data.Monoid
import Data.XML.Types (Event(..))
import Data.Text (Text)
import Data.Conduit
import Control.Monad.Trans.Control
import qualified Network.HTTP.Conduit as HTTP
import qualified Text.XML.Stream.Parse as XmlP
import Data.Time (UTCTime, formatTime, getCurrentTime)
import System.Locale (defaultTimeLocale, iso8601DateFormat)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Network.HTTP.Types as H
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.ByteString.Base64 as BASE
import Control.Monad.State

import AWS.EC2.Types
import AWS.Types
import AWS.Util
import AWS.EC2.Parser

{- Debug
import qualified Data.Conduit.Binary as CB
--}

data EC2Context = EC2Context
    { manager :: HTTP.Manager
    , credential :: Credential
    , endpoint :: EC2Endpoint
    }

type EC2 m = StateT EC2Context m

data QueryParams
    = ArrayParams ByteString [ByteString]
    | FilterParams [Filter]

type Filter = (ByteString, [ByteString])

queryHeader :: ByteString -> UTCTime -> Credential -> [(ByteString, ByteString)]
queryHeader action time cred =
    [ ("Action", action)
    , ("Version", apiVersion)
    , ("SignatureVersion", "2")
    , ("SignatureMethod", "HmacSHA256")
    , ("Timestamp", H.urlEncode True $ awsTimeFormat time)
    , ("AWSAccessKeyId", accessKey cred)
    ]

mkUrl :: Endpoint end
      => end
      -> Credential
      -> UTCTime
      -> ByteString
      -> [QueryParams]
      -> ByteString
mkUrl ep cred time action params = mconcat
    [ "https://"
    , endpointStr ep
    , "/?"
    , qparam
    , "&Signature="
    , signature ep (secretAccessKey cred) qparam
    ]
  where
    qheader = Map.fromList $ queryHeader action time cred
    qparam = queryStr $ Map.unions (qheader : map toArrayParams params)

toArrayParams :: QueryParams -> Map ByteString ByteString
toArrayParams (ArrayParams name params) = Map.fromList 
    [ (name <> "." <> bsShow i, param)
    | (i, param) <- zip [1..] params
    ]
toArrayParams (FilterParams fs) =
    Map.fromList . concat . map f1 $ zip [1..] fs
  where
    f1 (n, (name, vals)) = (filt n <> ".Name", name) :
        [ (filt n <> ".Value." <> bsShow i, param)
        | (i, param) <- zip [1..] vals
        ]
    filt n = "Filter." <> bsShow n

queryStr :: Map ByteString ByteString -> ByteString
queryStr = BS.intercalate "&" . Map.foldrWithKey' concatWithEqual []
  where
    concatWithEqual key val acc = key <> "=" <> val : acc

awsTimeFormat :: UTCTime -> ByteString
awsTimeFormat = BSC.pack . formatTime defaultTimeLocale (iso8601DateFormat $ Just "%XZ")

signature :: Endpoint end 
          => end -> SecretAccessKey -> ByteString -> ByteString
signature ep secret query = urlstr
  where
    stringToSign = mconcat
        [ "GET\n"
        , endpointStr ep
        , "\n/\n"
        , query
        ]
    signedStr = toS . SHA.bytestringDigest $ SHA.hmacSha256 (toL secret) (toL stringToSign)
    urlstr = H.urlEncode True . BASE.encode $ signedStr

sinkRequestId :: MonadThrow m
    => GLSink Event m Text
sinkRequestId = do
    await -- EventBeginDocument
    await -- EventBeginElement DescribeImagesResponse
    tagContentF "requestId"

ec2Query
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> [QueryParams]
    -> Conduit Event m o
    -> EC2 m (EC2Response (Source m o))
ec2Query action params cond = do
    ctx <- get
    let mgr = manager ctx
    let cred = credential ctx
    let ep = endpoint ctx
    time <- liftIO getCurrentTime
    let url = mkUrl ep cred time action params
    request <- liftIO $ HTTP.parseUrl (BSC.unpack url)
    lift $ do
        response <- HTTP.http request mgr
        (res, _) <- unwrapResumable $ HTTP.responseBody response
--        res $$ CB.sinkFile "debug.txt" >>= fail "debug"
        (src, rid) <- res $= XmlP.parseBytes XmlP.def $$+ sinkRequestId
        (src1, _) <- unwrapResumable src
        return $ EC2Response
            { requestId = rid
            , responseBody = src1 $= cond
            }
