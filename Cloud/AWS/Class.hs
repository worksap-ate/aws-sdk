{-# LANGUAGE GeneralizedNewtypeDeriving
 , TypeFamilies
 , FlexibleInstances
 , MultiParamTypeClasses
 , UndecidableInstances
 , DeriveDataTypeable
 , ExistentialQuantification
 , StandaloneDeriving
 #-}

module Cloud.AWS.Class
    ( AWS
    , runAWS
    , runAWSwithManager
    , defaultSettings
    , AWSException(..)
    , AWSContext(..)
    , AWSSettings(..)
    , getLastRequestId
      -- * re-export
    , monadThrow
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
import Control.Monad.Trans.Resource (monadThrow)
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Text.XML.Stream.Parse (XmlException)

import qualified Network.HTTP.Conduit as HTTP
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()

import Cloud.AWS.Credential

data AWSException
    = ClientError
        { clientErrorEndpoint :: ByteString
        , clientErrorAction :: ByteString
        , clientErrorStatus :: Int
        , clientErrorCode :: Text
        , clientErrorMessage :: Maybe Text
        , clientErrorRequestId :: Text
        } -- ^ This error is caused by client requests.
    | ServerError
        { serverErrorEndpoint :: ByteString
        , serverErrorAction :: ByteString
        , serverErrorStatus :: Int
        , serverErrorCode :: Text
        , serverErrorMessage :: Maybe Text
        , serverErrorRequestId :: Text
        } -- ^ This error is caused by server error in AWS.
    | ResponseParseError Text
    | FromTextError Text
        -- ^ parse error: cannot convert Text to oher data type.
    | XmlParserError XmlException
    | forall e . Exception e => ConnectionException e
    | forall e . Exception e => OtherInternalException e -- ^ bug
    | NextToken Text -- ^ This response has next token.
  deriving (Typeable)
deriving instance Show AWSException

instance Exception AWSException

data AWSContext = AWSContext
    { manager :: HTTP.Manager
    , endpoint :: ByteString
    , lastRequestId :: Maybe Text
    }

data AWSSettings = AWSSettings
    { credential :: Credential
    , httpTimeout :: Maybe Int
    }

defaultSettings :: Credential -> AWSSettings
defaultSettings cred = AWSSettings
    { credential = cred
    , httpTimeout = Just 60000000
    }

newtype AWS context m a = AWST
    { runAWST :: StateT context (ReaderT AWSSettings m) a
    } deriving
    ( Monad
    , Applicative
    , Functor
    , MonadIO
    , MonadState context
    , MonadReader AWSSettings
    , MonadBase base
    )

instance MonadTrans (AWS c)
  where
    lift = AWST . lift . lift

data StAWS a c = StAWS { unStAWS :: (a, c) }

instance MonadTransControl (AWS c)
  where
    type StT (AWS c) a = StAWS a c
    liftWith f = AWST . StateT $ \s -> ReaderT $ \r ->
        liftM (\x -> (x, s))
            (f $ \a -> liftM StAWS
                (R.runReaderT (S.runStateT (runAWST a) s) r))
    restoreT
        = AWST . StateT . const . ReaderT . const . liftM unStAWS

instance MonadBaseControl base m => MonadBaseControl base (AWS c m)
   where
     type StM (AWS c m) a = ComposeSt (AWS c) m a
     liftBaseWith = defaultLiftBaseWith
     restoreM = defaultRestoreM

runAWS :: MonadIO m
    => (HTTP.Manager -> c)
    -> AWS c m a
    -> m a
runAWS ctx app = do
    mgr <- liftIO $ HTTP.newManager HTTP.conduitManagerSettings
    cred <- liftIO $ loadCredential
    runAWSwithManager mgr ctx (defaultSettings cred) app

runAWSwithManager :: Monad m
    => HTTP.Manager
    -> (HTTP.Manager -> c)
    -> AWSSettings
    -> AWS c m a
    -> m a
runAWSwithManager mgr ctx settings app =
    R.runReaderT
        (S.evalStateT (runAWST app) $ ctx mgr) settings

getLastRequestId :: (Monad m, Functor m) => AWS AWSContext m (Maybe Text)
getLastRequestId = S.gets lastRequestId
