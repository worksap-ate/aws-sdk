{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.EC2.Query
    ( ec2Query
    , ec2QuerySource
    , ec2QuerySource'
    , ec2Request
    , QueryParam(..)
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Char8 as BSC

import Data.Monoid
import Data.XML.Types (Event(..), Name(..))
import Data.Conduit
import qualified Data.Conduit.List as CL
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
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader
import Control.Exception.Lifted as E
import Data.Text (Text)
import Control.Applicative

import AWS.EC2.Types
import AWS.Util
import AWS.EC2.Parser
import AWS.EC2.Class
import AWS.Credential

{- Debug
import Debug.Trace
import qualified Data.Conduit.Binary as CB
import System.IO (stdout)
--}

data QueryParam
    = ArrayParams Text [Text]
    | FilterParams [Filter]
    | ValueParam Text Text
    | StructArrayParams Text [[(Text, Text)]]
  deriving (Show)

queryHeader :: ByteString -> UTCTime -> Credential -> [(ByteString, ByteString)]
queryHeader action time cred =
    [ ("Action", action)
    , ("Version", apiVersion)
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
mkUrl ep cred time action params = mconcat
    [ "https://"
    , ep
    , "/?"
    , qparam
    , "&Signature="
    , signature ep (secretAccessKey cred) qparam
    ]
  where
    qheader = Map.fromList $ queryHeader action time cred
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

sinkRequestId :: MonadThrow m
    => GLSink Event m Text
sinkRequestId = do
    await -- EventBeginDocument
    await -- EventBeginElement DescribeImagesResponse
    tagContentF "requestId"

checkStatus' ::
    H.Status -> H.ResponseHeaders -> Maybe SomeException
checkStatus' = \s@(H.Status sci _) hs ->
    if 200 <= sci && sci < 300 || sci == 400
        then Nothing
        else Just $ toException $ HTTP.StatusCodeException s hs

sinkError :: MonadThrow m => GLSink Event m a
sinkError = do
    await
    etag "Response" $ do
        (c,m) <- etag "Errors" $ etag "Error" $
            (,) <$> tagt "Code" <*> tagt "Message"
        r <- tagt "RequestID"
        lift $ monadThrow $ ClientError c m r
  where
    etag name inner = XmlP.force "error parse error"
        $ XmlP.tagNoAttr (errName name) inner
    tagt name = etag name XmlP.content
    errName n = Name n Nothing Nothing

clientError
    :: (MonadResource m, MonadBaseControl IO m)
    => ResumableSource m ByteString -> m a
clientError rsrc =
    rsrc $$+- XmlP.parseBytes XmlP.def =$ sinkError

ec2Request
    :: (MonadResource m, MonadBaseControl IO m)
    => Credential
    -> EC2Context
    -> ByteString
    -> [QueryParam]
    -> m (ResumableSource m ByteString)
ec2Request cred ctx action params = do
    let mgr = manager ctx
    let ep = endpoint ctx
    time <- liftIO getCurrentTime
    let url = mkUrl ep cred time action params
    request <- liftIO $ HTTP.parseUrl (BSC.unpack url)
    let req = request { HTTP.checkStatus = checkStatus' }
    response <- HTTP.http req mgr
    let body = HTTP.responseBody response
    if (H.statusCode $ HTTP.responseStatus response) == 400
        then clientError body
        else return ()
    return body

ec2Query
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> [QueryParam]
    -> GLSink Event m o
    -> EC2 m o
ec2Query action params sink = do
    src <- ec2QuerySource action params $ sink >>= yield
    lift (src $$ CL.head) >>= maybe (fail "parse error") return

ec2QuerySource
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> [QueryParam]
    -> Conduit Event m o
    -> EC2 m (Source m o)
ec2QuerySource action params cond = do
    ec2QuerySource' action params Nothing cond

ec2QuerySource'
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> [QueryParam]
    -> Maybe Text
    -> Conduit Event m o
    -> EC2 m (Source m o)
ec2QuerySource' action params token cond = do
    cred <- Reader.ask
    ctx <- State.get
    (src1, rid) <- lift $ do
        response <- ec2Request cred ctx action params'
        (res, _) <- unwrapResumable response
--        res $$ CB.sinkFile "debug.txt" >>= fail "debug"
        res $= XmlP.parseBytes XmlP.def $$+ sinkRequestId
    State.put ctx{lastRequestId = Just rid}
    lift $ do
        (src2, _) <- unwrapResumable src1
        return $ src2 $= (cond >> nextToken)
  where
    params' = maybe params
        (\t -> ValueParam "NextToken" t:params) token

    nextToken
        :: (MonadResource m, MonadBaseControl IO m)
        => Conduit Event m o
    nextToken = do
        mt <- getMT "nextToken"
        case mt of
            Nothing -> return ()
            Just t  -> E.throw $ NextToken t
