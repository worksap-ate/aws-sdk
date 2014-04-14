{-# LANGUAGE FlexibleContexts, RankNTypes, CPP #-}

module AWS.Lib.Query
    ( QueryParam
    , Filter
    , showUnitParams
    , (|.+)
    , putNumberV, putNumberP
    , filtersParam
    , maybeParam
    , nothingParam
    , (|=), (|.)
    , (|=?), (|.?)
    , (|.#=), (|.#.)
#if !MIN_VERSION_conduit(1,0,16)
    , ($=+)
#endif
    , requestQuery
    , commonQuery
    , exceptionTransform
    , textToBS
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import qualified Data.Text as T

import Data.List (transpose)
import Data.Maybe
import Data.Monoid
import Data.XML.Types (Event(..))
import Data.Conduit
#if !MIN_VERSION_conduit(1,0,16)
import qualified Data.Conduit.Internal as CI
#endif
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
#endif
import qualified Network.HTTP.Conduit as HTTP
import qualified Text.XML.Stream.Parse as XmlP
import Text.XML.Stream.Parse (XmlException)
import Data.Time (UTCTime, formatTime, getCurrentTime)
import System.Locale (defaultTimeLocale, iso8601DateFormat)
import qualified Data.Map as Map
import Data.Map (Map)
import Network.HTTP.Conduit (HttpException)
import qualified Network.HTTP.Types as H
import Network.TLS (TLSException)
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.ByteString.Base64 as BASE
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Exception.Lifted as E
import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader

import AWS.Class
import AWS.Util
import AWS.Credential
import AWS.Lib.Parser
import AWS.EC2.Types (Filter)

#ifdef DEBUG
import qualified System.IO as IO
#endif

data QueryParam
    = Leaf Text Text
    | Inner Text [QueryParam]

instance Show QueryParam where
    show = show . paramToText

paramToText :: QueryParam -> Text
paramToText (Leaf k v) = k <> "=" <> v
paramToText p@(Inner _ _) =
    T.intercalate "&" . map paramToText $ partition p

showUnitParams :: [QueryParam] -> [String]
showUnitParams = map show . concat . map partition

infixr 3 |.+
(|.+) :: Text -> QueryParam -> QueryParam
t |.+ (Leaf k v) = t <> "." <> k |= v
t |.+ (Inner k ps) = t <> "." <> k |. ps

paramsToMap :: [QueryParam] -> Map ByteString ByteString
paramsToMap = Map.fromList . map tup . concat . map partition
  where
    tup (Leaf k v) = (textToBS k, textToBS v)
    tup (Inner _ _) = error "partition param error"

-- | partition to unit
partition :: QueryParam -> [QueryParam]
partition p@(Leaf _ _) = [p]
partition (Inner k ps) = concat $ map (partition . (k |.+)) ps

-- | put a number to each value
putNumberV :: [Text] -> [QueryParam]
putNumberV = map (uncurry Leaf) . zip (map toText ([1..] :: [Int]))

-- | put a number to each params
putNumberP :: [[QueryParam]] -> [QueryParam]
putNumberP = map (uncurry Inner) . zip (map toText ([1..] :: [Int]))

filtersParam :: [Filter] -> QueryParam
filtersParam filters = "Filter" |.#. transpose [keyParams, valParams]
  where
    keyParams = map (("Name" |=) . fst) filters
    valParams = map (("Value" |.#=) . snd) filters

maybeParam :: Maybe QueryParam -> QueryParam
maybeParam (Just p) = p
maybeParam Nothing = nothingParam

nothingParam :: QueryParam
nothingParam = Inner "" []

infixr 3 |=
(|=) :: Text -> Text -> QueryParam
(|=) = Leaf

infixr 3 |.
(|.) :: Text -> [QueryParam] -> QueryParam
(|.) = Inner

infixr 3 |=?
(|=?) :: Text -> Maybe Text -> QueryParam
t |=? (Just a) = t |= a
_ |=? Nothing = nothingParam

infixr 3 |.?
(|.?) :: Text -> Maybe [QueryParam] -> QueryParam
t |.? (Just ps) = t |. ps
_ |.? Nothing = nothingParam

infixr 3 |.#=
(|.#=) :: Text -> [Text] -> QueryParam
t |.#= ts = t |. putNumberV ts

infixr 3 |.#.
(|.#.) :: Text -> [[QueryParam]] -> QueryParam
t |.#. ps = t |. putNumberP ps

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
    , ("Timestamp", awsTimeFormat time)
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
    qparam = queryStr $ Map.union qheader $ paramsToMap params

textToBS :: Text -> ByteString
textToBS = BSC.pack . T.unpack

queryStr :: Map ByteString ByteString -> ByteString
queryStr = BS.intercalate "&" . Map.foldrWithKey' concatWithEqual []
  where
    concatWithEqual key val acc
        = key
        <> "="
        <> (H.urlEncode True val) : acc

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
    H.Status -> H.ResponseHeaders -> HTTP.CookieJar -> Maybe SomeException
checkStatus' = \s@(H.Status sci _) hs cookie ->
    if 200 <= sci && sci < 300 || 400 <= sci
        then Nothing
        else Just $ toException $ HTTP.StatusCodeException s hs cookie

clientError
    :: (MonadResource m, MonadBaseControl IO m)
    => Int
    -> ResumableSource m ByteString
    -> (Int -> Consumer Event m a)
    -> m a
clientError status rsrc errSink =
    rsrc $$+- XmlP.parseBytes XmlP.def =$ errSink status

#if !MIN_VERSION_conduit(1,0,16)
($=+) :: MonadIO m
    => ResumableSource m a
    -> Conduit a m b
    -> m (ResumableSource m b)
CI.ResumableSource src final $=+ cond = CI.ResumableSource (src $= conduit) final
#endif

requestQuery
    :: (MonadResource m, MonadBaseControl IO m)
    => AWSSettings
    -> AWSContext
    -> ByteString
    -> [QueryParam]
    -> ByteString
    -> (ByteString -> Int -> Consumer Event m a)
    -> m (ResumableSource m ByteString)
requestQuery settings ctx action params ver errSink = do
    let mgr = manager ctx
    let ep = endpoint ctx
    let cred = credential settings
    time <- liftIO getCurrentTime
    let url = mkUrl ep cred time action params ver
    request <- liftIO $ HTTP.parseUrl (BSC.unpack url)
    let req = request
            { HTTP.checkStatus = checkStatus'
            , HTTP.responseTimeout = httpTimeout settings
            }
    response <- HTTP.http req mgr
    let body = HTTP.responseBody response
    let st = H.statusCode $ HTTP.responseStatus response
    if st < 400
#ifdef DEBUG
        then return $ body $=+ conduitLog "aws-sdk.log" url
        else do
            let body' = body $=+ conduitLog "aws-sdk-error.log" url
            clientError st body' $ errSink action
            fail "not reached"
#else
        then return body
        else do
            clientError st body $ errSink action
            fail "not reached"
#endif

#ifdef DEBUG
conduitLog :: MonadResource m => FilePath -> ByteString -> Conduit ByteString m ByteString
conduitLog path url = bracketP (E.try $ IO.openBinaryFile path IO.AppendMode) release go
  where
    release :: Either SomeException IO.Handle -> IO ()
    release (Left _) = return ()
    release (Right h) = do
        liftIO $ BSC.hPutStrLn h ""
        IO.hClose h

    go :: MonadResource m => Either SomeException IO.Handle -> Conduit ByteString m ByteString
    go (Left _) = awaitForever yield
    go (Right h) = do
        liftIO $ do
            time <- getCurrentTime
            BSC.hPutStrLn h $ "[" <> awsTimeFormat time <> "] " <> url
        awaitForever $ \bs -> liftIO (BS.hPut h bs) >> yield bs
#endif

commonQuery
    :: (MonadBaseControl IO m, MonadResource m)
    => ByteString -- ^ apiVersion
    -> ByteString -- ^ Action
    -> [QueryParam]
    -> Consumer Event m a
    -> AWS AWSContext m a
commonQuery apiVersion action params sink = do
    ctx <- State.get
    settings <- Reader.ask
    (res, rid) <- lift $ E.handle exceptionTransform $ do
        rs <- requestQuery settings ctx action params apiVersion sinkError
        rs $$+- XmlP.parseBytes XmlP.def
           =$   sinkResponse (bsToText action) sink
    State.put ctx { lastRequestId = Just rid }
    return res

exceptionTransform
    :: (MonadBaseControl IO m, MonadResource m)
    => SomeException -> m a
exceptionTransform e
    | isJust awse  = throw $ fromJust awse
    | isJust xmle  = throw $ XmlParserError $ fromJust xmle
    | isJust httpe = throw $ ConnectionException $ fromJust httpe
    | isJust tlse  = throw $ ConnectionException $ fromJust tlse
    | isJust ioe   = throw $ ConnectionException $ fromJust ioe
    | otherwise    = throw $ OtherInternalException e
  where
    awse = fromException e :: Maybe AWSException
    xmle = fromException e :: Maybe XmlException
    httpe = fromException e :: Maybe HttpException
    tlse  = fromException e :: Maybe TLSException
    ioe   = fromException e :: Maybe IOException
