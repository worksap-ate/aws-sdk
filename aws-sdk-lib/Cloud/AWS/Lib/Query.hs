{-# LANGUAGE FlexibleContexts, RankNTypes, CPP #-}

module Cloud.AWS.Lib.Query
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
    , requestQuery
    , commonQuery
    , exceptionTransform
    , textToBS
    ) where

import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import qualified Data.Text as T

import Data.List (transpose)
import Data.Maybe
import Data.Monoid
import Data.XML.Types (Event(..))
import Data.Conduit
import qualified Network.HTTP.Conduit as HTTP
import qualified Text.XML.Stream.Parse as XmlP
import Text.XML.Stream.Parse (XmlException)
import Data.Time (UTCTime, formatTime, getCurrentTime)
import System.Locale (defaultTimeLocale, iso8601DateFormat)
import Network.HTTP.Conduit (HttpException)
import qualified Network.HTTP.Rest.Signature.EC2 as Sign
import qualified Network.HTTP.Types as H
#if MIN_VERSION_tls(1,2,0)
import Network.TLS (TLSException)
#else
import Network.TLS (HandshakeFailed)
#endif
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
import Control.Exception.Lifted as E
import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader

import Cloud.AWS.Lib.Parser.Unordered (XmlElement)
import Cloud.AWS.Lib.ToText (ToText(toText))

import Cloud.AWS.Class
import Cloud.AWS.Credential
import Cloud.AWS.Lib.Parser (sinkError, sinkResponse)

#ifdef DEBUG
import qualified System.IO as IO
#endif

type Filter = (Text, [Text])

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

-- | partition to unit
partition :: QueryParam -> [QueryParam]
partition p@(Leaf _ _) = [p]
partition (Inner k ps) = concat $ map (partition . (k |.+)) ps

-- | put a number to each value
putNumberV :: ToText a => [a] -> [QueryParam]
putNumberV
    = map (uncurry Leaf)
    . zip (map toText ([1..] :: [Int]))
    . map toText

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
(|=) :: ToText a => Text -> a -> QueryParam
(|=) a = Leaf a . toText

infixr 3 |.
(|.) :: Text -> [QueryParam] -> QueryParam
(|.) = Inner

infixr 3 |=?
(|=?) :: ToText a => Text -> Maybe a -> QueryParam
t |=? (Just a) = t |= a
_ |=? Nothing = nothingParam

infixr 3 |.?
(|.?) :: Text -> Maybe [QueryParam] -> QueryParam
t |.? (Just ps) = t |. ps
_ |.? Nothing = nothingParam

infixr 3 |.#=
(|.#=) :: ToText a => Text -> [a] -> QueryParam
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
    , "/"
    , H.renderSimpleQuery True qparam
    , "&Signature="
    , Sign.signature "GET" ep "/" (secretAccessKey cred) Sign.HmacSHA256 qparam
    ]
  where
    qheader = queryHeader action time cred ver
    qparam = qheader ++ f params
    f = map tup . concat . map partition
    tup (Leaf k v) = (textToBS k, textToBS v)
    tup (Inner _ _) = error "partition param error"

textToBS :: Text -> ByteString
textToBS = BSC.pack . T.unpack

awsTimeFormat :: UTCTime -> ByteString
awsTimeFormat = BSC.pack . formatTime defaultTimeLocale (iso8601DateFormat $ Just "%XZ")

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

requestQuery
    :: (MonadResource m, MonadBaseControl IO m)
    => AWSSettings
    -> AWSContext
    -> ByteString
    -> [QueryParam]
    -> ByteString
    -> (ByteString -> ByteString -> Int -> Consumer Event m a)
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
            clientError st body' $ errSink ep action
            fail "not reached"
#else
        then return body
        else do
            clientError st body $ errSink ep action
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
        awaitForever $ \bs -> liftIO (BSC.hPut h bs) >> yield bs
#endif

commonQuery
    :: (MonadBaseControl IO m, MonadResource m)
    => ByteString -- ^ apiVersion
    -> ByteString -- ^ Action
    -> [QueryParam]
    -> (XmlElement -> m a)
    -> AWS AWSContext m a
commonQuery apiVersion action params parser = do
    ctx <- State.get
    settings <- Reader.ask
    (res, rid) <- lift $ E.handle exceptionTransform $ do
        rs <- requestQuery settings ctx action params apiVersion sinkError
        rs $$+- XmlP.parseBytes XmlP.def
           =$   sinkResponse (toText action) parser
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
#if MIN_VERSION_tls(1,2,0)
    tlse  = fromException e :: Maybe TLSException
#else
    tlse  = fromException e :: Maybe HandshakeFailed
#endif
    ioe   = fromException e :: Maybe IOException
