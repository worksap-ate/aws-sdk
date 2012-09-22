{-# LANGUAGE GeneralizedNewtypeDeriving
 , TypeFamilies
 , FlexibleInstances
 , MultiParamTypeClasses
 , UndecidableInstances
 #-}

module AWS.EC2.Class
    ( EC2
    , runEC2
    ) where

import Control.Monad.State (StateT(..), MonadState)
import qualified Control.Monad.State as S
import Control.Applicative
import Control.Monad (liftM)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control
    ( MonadBaseControl(..)
    , MonadTransControl(..)
    , ComposeSt
    , defaultLiftBaseWith
    , defaultRestoreM
    )

import AWS.EC2.Types

newtype EC2 m a = EC2T
    { runEC2T :: StateT EC2Context m a
    } deriving
    ( Monad
    , Applicative
    , Functor
    , MonadIO
    , MonadState EC2Context
    , MonadBase base
    )

instance MonadTrans EC2 where
    lift = EC2T . lift

instance MonadTransControl EC2 where
    newtype StT EC2 a = StEC2 { unStEC2 :: (a, EC2Context) }
    liftWith f = EC2T . StateT $ \s ->
        liftM (\x -> (x, s))
            (f $ \a -> liftM StEC2
                (S.runStateT (runEC2T a) s))
    restoreT = EC2T . StateT . const . liftM unStEC2

instance MonadBaseControl base m => MonadBaseControl base (EC2 m) where
    newtype StM (EC2 m) a = StMEC2 { unStMEC2 :: ComposeSt EC2 m a }
    liftBaseWith = defaultLiftBaseWith StMEC2
    restoreM = defaultRestoreM unStMEC2

runEC2 :: Monad m => EC2Context -> EC2 m a -> m a
runEC2 ctx app = S.evalStateT (runEC2T app) ctx
