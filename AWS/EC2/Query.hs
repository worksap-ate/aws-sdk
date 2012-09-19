{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module AWS.EC2.Query
    ( ec2Query
    , ec2Request
    , QueryParams(..)
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Char8 as BSC

import Data.Monoid
import Data.XML.Types (Event(..))
import Data.Text (Text)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Network.HTTP.Conduit as HTTP
import qualified Text.XML.Stream.Parse as XmlP
import Data.Time (UTCTime, formatTime, getCurrentTime)
import System.Locale (defaultTimeLocale, iso8601DateFormat)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Network.HTTP.Types as H
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.ByteString.Base64 as BASE
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.State as ST
import Data.Typeable (Typeable)
import Control.Exception.Lifted as E

import AWS.EC2.Types
import AWS.Types
import AWS.Util
import AWS.EC2.Parser

{- Debug
import Debug.Trace
import qualified Data.Conduit.Binary as CB
--}

data ResponseParserException
    = NextToken Text
  deriving (Show, Typeable)

instance Exception ResponseParserException

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
toArrayParams (ValueParam k v) = Map.singleton k v

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
    stringToSign = "GET\n" <> endpointStr ep <> "\n/\n" <> query
    signedStr = toS . SHA.bytestringDigest $ SHA.hmacSha256 (toL secret) (toL stringToSign)
    urlstr = H.urlEncode True . BASE.encode $ signedStr

sinkRequestId :: MonadThrow m
    => GLSink Event m Text
sinkRequestId = do
    await -- EventBeginDocument
    await -- EventBeginElement DescribeImagesResponse
    tagContentF "requestId"

ec2Request
    :: (MonadResource m, MonadBaseControl IO m)
    => EC2Context
    -> ByteString
    -> [QueryParams]
    -> m (ResumableSource m ByteString)
ec2Request ctx action params = do
    let mgr = manager ctx
    let cred = credential ctx
    let ep = endpoint ctx
    time <- liftIO getCurrentTime
    let url = mkUrl ep cred time action params
    request <- liftIO $ HTTP.parseUrl (BSC.unpack url)
    response <- HTTP.http request mgr
    return $ HTTP.responseBody response

ec2Query
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> [QueryParams]
    -> Conduit Event m o
    -> EC2 m (Source m o)
ec2Query action params cond = do
    ctx <- ST.get
    (src1, rid) <- lift $ do
        response <- ec2Request ctx action params
        (res, _) <- unwrapResumable response
--        res $$ CB.sinkFile "debug.txt" >>= fail "debug"
        res $= XmlP.parseBytes XmlP.def $$+ sinkRequestId
    ST.put ctx{lastRequestId = Just rid}
    lift $ do
        (src2, _) <- unwrapResumable src1
        return $ src2 $= cond
