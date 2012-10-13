{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.Lib.Query
    ( requestQuery
    , QueryParam(..)
    , Filter
    , maybeParams
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Char8 as BSC

import Data.Monoid
import Data.XML.Types (Event(..))
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
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception.Lifted as E
import Data.Text (Text)

import AWS.Class
import AWS.Util
import AWS.Credential

type Filter = (Text, [Text])

data QueryParam
    = ArrayParams Text [Text]
    | FilterParams [Filter]
    | ValueParam Text Text
    | StructArrayParams Text [[(Text, Text)]]
  deriving (Show)

queryHeader
    :: ByteString
    -> UTCTime
    -> Credential
    -> ByteString
    -> [(ByteString, ByteString)]
queryHeader action time cred ver =
    [ ("Action", action)
    , ("Version", ver)
    , ("SignatureVersion", "2")
    , ("SignatureMethod", "HmacSHA256")
    , ("Timestamp", H.urlEncode True $ awsTimeFormat time)
    , ("AWSAccessKeyId", accessKey cred)
    ]

mkUrl :: ByteString
      -> Credential
      -> UTCTime
      -> ByteString
      -> [QueryParam]
      -> ByteString
      -> ByteString
mkUrl ep cred time action params ver = mconcat
    [ "https://"
    , ep
    , "/?"
    , qparam
    , "&Signature="
    , signature ep (secretAccessKey cred) qparam
    ]
  where
    qheader = Map.fromList $ queryHeader action time cred ver
    qparam = queryStr $ Map.unions (qheader : map toArrayParams params)

toArrayParams :: QueryParam -> Map ByteString ByteString
toArrayParams (ArrayParams name params) = Map.fromList 
    [ (textToBS name <> "." <> bsShow i, textToBS param)
    | (i, param) <- zip ([1..]::[Int]) params

    ]
toArrayParams (FilterParams kvs) =
    Map.fromList . concat . map f1 $ zip ([1..]::[Int]) kvs
  where
    f1 (n, (key, vals)) = (filt n <> ".Name", textToBS key) :
        [ (filt n <> ".Value." <> bsShow i, textToBS param)
        | (i, param) <- zip ([1..]::[Int]) vals
        ]
    filt n = "Filter." <> bsShow n
toArrayParams (ValueParam k v) =
    Map.singleton (textToBS k) (textToBS v)
toArrayParams (StructArrayParams name vss) = Map.fromList l
  where
    bsName = textToBS name
    struct n (k, v) = (n <> "." <> textToBS k, textToBS v)
    l = mconcat
        [ map (struct (bsName <> "." <> bsShow i)) kvs
        | (i, kvs) <- zip ([1..]::[Int]) vss
        ]

queryStr :: Map ByteString ByteString -> ByteString
queryStr = BS.intercalate "&" . Map.foldrWithKey' concatWithEqual []
  where
    concatWithEqual key val acc = key <> "=" <> val : acc

awsTimeFormat :: UTCTime -> ByteString
awsTimeFormat = BSC.pack . formatTime defaultTimeLocale (iso8601DateFormat $ Just "%XZ")

signature
    :: ByteString -> SecretAccessKey -> ByteString -> ByteString
signature ep secret query = urlstr
  where
    stringToSign = "GET\n" <> ep <> "\n/\n" <> query
    signedStr = toS . SHA.bytestringDigest $ SHA.hmacSha256 (toL secret) (toL stringToSign)
    urlstr = H.urlEncode True . BASE.encode $ signedStr

checkStatus' ::
    H.Status -> H.ResponseHeaders -> Maybe SomeException
checkStatus' = \s@(H.Status sci _) hs ->
    if 200 <= sci && sci < 300 || 400 <= sci
        then Nothing
        else Just $ toException $ HTTP.StatusCodeException s hs

clientError
    :: (MonadResource m, MonadBaseControl IO m)
    => Int
    -> ResumableSource m ByteString
    -> (Int -> GLSink Event m a)
    -> m a
clientError status rsrc errSink =
    rsrc $$+- XmlP.parseBytes XmlP.def =$ errSink status

requestQuery
    :: (MonadResource m, MonadBaseControl IO m)
    => Credential
    -> AWSContext
    -> ByteString
    -> [QueryParam]
    -> ByteString
    -> (Int -> GLSink Event m a)
    -> m (ResumableSource m ByteString)
requestQuery cred ctx action params ver errSink = do
    let mgr = manager ctx
    let ep = endpoint ctx
    time <- liftIO getCurrentTime
    let url = mkUrl ep cred time action params ver
    request <- liftIO $ HTTP.parseUrl (BSC.unpack url)
    let req = request { HTTP.checkStatus = checkStatus' }
    response <- HTTP.http req mgr
    let body = HTTP.responseBody response
    let st = H.statusCode $ HTTP.responseStatus response
    if st < 400
        then return body
        else do
            clientError st body errSink
            fail "not reached"

maybeParams :: [(Text, Maybe Text)] -> [QueryParam]
maybeParams params = params >>= uncurry mk
  where
    mk name = maybe [] (\a -> [ValueParam name a])
