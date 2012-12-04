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
    ) where

import Control.Monad.State (StateT(..), MonadState)
import qualified Control.Monad.State as S
import Control.Monad.Reader (ReaderT(..), MonadReader)
import qualified Control.Monad.Reader as R
import Control.Applicative
import Control.Monad (liftM)
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

import qualified Network.HTTP.Conduit as HTTP
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()

import AWS.Credential

data AWSException
    = ClientError
        { errorAction :: ByteString
        , errorStatus :: Int
        , errorCode :: Text
        , errorMessage :: Text
        , errorRequestId :: Text
        } -- ^ This error is caused by client requests.
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
