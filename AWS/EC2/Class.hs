{-# LANGUAGE GeneralizedNewtypeDeriving
 , TypeFamilies
 , FlexibleInstances
 , MultiParamTypeClasses
 , UndecidableInstances
 , DeriveDataTypeable
 #-}

module AWS.EC2.Class
    ( EC2
    , runEC2
    , ResponseParserException(..)
    , EC2Context(..)
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

import AWS.Types
import AWS.Credential

data EC2Context = EC2Context
    { manager :: HTTP.Manager
    , endpoint :: EC2Endpoint
    , lastRequestId :: Maybe Text
    }

initialEC2Context :: HTTP.Manager -> EC2Context
initialEC2Context mgr = EC2Context
    { manager = mgr
    , endpoint = UsEast1
    , lastRequestId = Nothing
    }

data ResponseParserException
    = NextToken Text
  deriving (Show, Typeable)

instance Exception ResponseParserException

newtype EC2 m a = EC2T
    { runEC2T :: StateT EC2Context (ReaderT Credential m) a
    } deriving
    ( Monad
    , Applicative
    , Functor
    , MonadIO
    , MonadState EC2Context
    , MonadReader Credential
    , MonadBase base
    )

instance MonadTrans EC2 where
    lift = EC2T . lift . lift

instance MonadTransControl EC2 where
    newtype StT EC2 a = StEC2 { unStEC2 :: (a, EC2Context) }
    liftWith f = EC2T . StateT $ \s -> ReaderT $ \r ->
        liftM (\x -> (x, s))
            (f $ \a -> liftM StEC2
                (R.runReaderT (S.runStateT (runEC2T a) s) r))
    restoreT = EC2T . StateT . const . ReaderT . const . liftM unStEC2

instance MonadBaseControl base m => MonadBaseControl base (EC2 m) where
    newtype StM (EC2 m) a = StMEC2 { unStMEC2 :: ComposeSt EC2 m a }
    liftBaseWith = defaultLiftBaseWith StMEC2
    restoreM = defaultRestoreM unStMEC2

runEC2 :: MonadIO m => Credential -> EC2 m a -> m a
runEC2 cred app = do
    mgr <- liftIO $ HTTP.newManager HTTP.def
    R.runReaderT
        (S.evalStateT (runEC2T app) $ initialEC2Context mgr)
        cred
