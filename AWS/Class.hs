{-# LANGUAGE GeneralizedNewtypeDeriving
 , TypeFamilies
 , FlexibleInstances
 , MultiParamTypeClasses
 , UndecidableInstances
 , DeriveDataTypeable
 , ExistentialQuantification
 , StandaloneDeriving
 , CPP
 #-}

module AWS.Class
    ( AWS
    , runAWS
    , runAWSwithManager
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
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (monadThrow)
#endif
import Control.Exception (Exception)
import Data.Typeable (Typeable)
#if !MIN_VERSION_conduit(1,1,0)
import Data.Conduit (MonadThrow, monadThrow)
#endif
import Text.XML.Stream.Parse (XmlException)

import qualified Network.HTTP.Conduit as HTTP
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()

import AWS.Credential

data AWSException
    = ClientError
        { clientErrorAction :: ByteString
        , clientErrorStatus :: Int
        , clientErrorCode :: Text
        , clientErrorMessage :: Maybe Text
        , clientErrorRequestId :: Text
        } -- ^ This error is caused by client requests.
    | ServerError
        { serverErrorAction :: ByteString
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

runAWS :: MonadIO m
    => (HTTP.Manager -> c)
    -> Credential
    -> AWS c m a
    -> m a
runAWS ctx cred app = do
    mgr <- liftIO $ HTTP.newManager HTTP.conduitManagerSettings
    runAWSwithManager mgr ctx cred app

runAWSwithManager :: Monad m
    => HTTP.Manager
    -> (HTTP.Manager -> c)
    -> Credential
    -> AWS c m a
    -> m a
runAWSwithManager mgr ctx cred app =
    R.runReaderT
        (S.evalStateT (runAWST app) $ ctx mgr)
        $ AWSSettings cred (Just 60000000)

getLastRequestId :: (Monad m, Functor m) => AWS AWSContext m (Maybe Text)
getLastRequestId = lastRequestId <$> S.get
