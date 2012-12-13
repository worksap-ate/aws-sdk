{-# LANGUAGE GeneralizedNewtypeDeriving
 , TypeFamilies
 , FlexibleInstances
 , MultiParamTypeClasses
 , UndecidableInstances
 , DeriveDataTypeable
 #-}

module AWS.Class
    ( AWS
    , runAWS
    , AWSException(..)
    , AWSContext(..)
    , getLastRequestId
    , FromText(..)
    ) where

import Control.Monad.State (StateT(..), MonadState)
import qualified Control.Monad.State as S
import Control.Monad.Reader (ReaderT(..), MonadReader)
import qualified Control.Monad.Reader as R
import Control.Applicative
import Control.Monad
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control
    ( MonadBaseControl(..)
    , MonadTransControl(..)
    , ComposeSt
    , defaultLiftBaseWith
    , defaultRestoreM
    )
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Data.Conduit (MonadThrow, monadThrow)
import Safe (readMay)
import qualified Data.Time.Parse as TP

import qualified Network.HTTP.Conduit as HTTP
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Data.IP (IPv4, AddrRange)
import Data.Time (UTCTime)
import qualified Data.Time as Time

import AWS.Credential

data AWSException
    = ClientError
        { errorAction :: ByteString
        , errorStatus :: Int
        , errorCode :: Text
        , errorMessage :: Text
        , errorRequestId :: Text
        } -- ^ This error is caused by client requests.
    | TextConversionException Text
        -- ^ parse error: cannot convert Text to oher data type.
    | NextToken Text -- ^ This response has next token.
  deriving (Show, Typeable)

instance Exception AWSException

data AWSContext = AWSContext
    { manager :: HTTP.Manager
    , endpoint :: ByteString
    , lastRequestId :: Maybe Text
    }

newtype AWS context m a = AWST
    { runAWST :: StateT context (ReaderT Credential m) a
    } deriving
    ( Monad
    , Applicative
    , Functor
    , MonadIO
    , MonadState context
    , MonadReader Credential
    , MonadBase base
    )

instance MonadTrans (AWS c)
  where
    lift = AWST . lift . lift

instance MonadTransControl (AWS c)
  where
    newtype StT (AWS c) a = StAWS { unStAWS :: (a, c) }
    liftWith f = AWST . StateT $ \s -> ReaderT $ \r ->
        liftM (\x -> (x, s))
            (f $ \a -> liftM StAWS
                (R.runReaderT (S.runStateT (runAWST a) s) r))
    restoreT
        = AWST . StateT . const . ReaderT . const . liftM unStAWS

instance MonadBaseControl base m => MonadBaseControl base (AWS c m)
  where
    newtype StM (AWS c m) a
        = StMAWS { unStMAWS :: ComposeSt (AWS c) m a }
    liftBaseWith = defaultLiftBaseWith StMAWS
    restoreM = defaultRestoreM unStMAWS

class Read a => FromText a
  where
    fromText :: MonadThrow m => Text -> m a
    fromText t
        = maybe (monadThrow $ TextConversionException t) return
        . fromTextMay
        $ t

    fromTextMay :: Text -> Maybe a
    fromTextMay = readMay . T.unpack

    fromMaybeText :: MonadThrow m => Maybe Text -> m a
    fromMaybeText
        = maybe
            (monadThrow $ TextConversionException "no text")
            fromText

instance FromText a => FromText (Maybe a)
  where
    fromText = return . join . fromTextMay
    fromMaybeText Nothing  = return Nothing
    fromMaybeText (Just t) = fromText t >>= return . Just
    fromTextMay = Just . fromTextMay

instance FromText Int
instance FromText Integer
instance FromText Double
instance FromText IPv4
instance FromText (AddrRange IPv4)

instance FromText Text
  where
    fromTextMay = Just

instance FromText Bool
  where
    fromTextMay "true"  = Just True
    fromTextMay "false" = Just False
    fromTextMay _       = Nothing

instance FromText UTCTime
  where
    fromTextMay t
        = Time.localTimeToUTC Time.utc . fst
        <$> (TP.strptime fmt $ T.unpack t)
      where
        fmt = "%FT%T"

runAWS :: MonadIO m
    => (HTTP.Manager -> c)
    -> Credential
    -> AWS c m a
    -> m a
runAWS ctx cred app = do
    mgr <- liftIO $ HTTP.newManager HTTP.def
    R.runReaderT
        (S.evalStateT (runAWST app) $ ctx mgr)
        cred

getLastRequestId :: (Monad m, Functor m) => AWS AWSContext m (Maybe Text)
getLastRequestId = lastRequestId <$> S.get
