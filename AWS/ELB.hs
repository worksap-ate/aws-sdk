{-# LANGUAGE FlexibleContexts, RankNTypes, CPP #-}

module AWS.ELB
    ( -- * ELB Environment
      ELB
    , runELB
    , runELBwithManager
    , setRegion
    , apiVersion
      -- * LoadBalancer
    , module AWS.ELB.LoadBalancer
    ) where

import Data.Text (Text)
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadBaseControl, MonadResource)
#else
import Data.Conduit
#endif
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.State as State
import qualified Network.HTTP.Conduit as HTTP
import Data.Monoid

import AWS.Class
import AWS.Lib.Query (textToBS)

import AWS
import AWS.ELB.Internal
import AWS.ELB.LoadBalancer

initialELBContext :: HTTP.Manager -> AWSContext
initialELBContext mgr = AWSContext
    { manager = mgr
    , endpoint = "elasticloadbalancing.amazonaws.com"
    , lastRequestId = Nothing
    }

runELB :: MonadIO m => Credential -> ELB m a -> m a
runELB = runAWS initialELBContext

runELBwithManager :: Monad m
    => HTTP.Manager -> Credential -> ELB m a -> m a
runELBwithManager mgr = runAWSwithManager mgr initialELBContext

setRegion
    :: (MonadBaseControl IO m, MonadResource m)
    => Text -> ELB m ()
setRegion region = do
    ctx <- State.get
    State.put
        ctx { endpoint =
            "elasticloadbalancing." <> textToBS region <> ".amazonaws.com"
            }
