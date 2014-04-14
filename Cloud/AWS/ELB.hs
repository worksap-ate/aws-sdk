{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Cloud.AWS.ELB
    ( -- * ELB Environment
      ELB
    , runELB
    , runELBwithManager
    , setRegion
    , apiVersion
      -- * LoadBalancer
    , module Cloud.AWS.ELB.LoadBalancer
    ) where

import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.State as State
import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
import qualified Network.HTTP.Conduit as HTTP
import Data.Monoid

import Cloud.AWS.Class
import Cloud.AWS.Lib.Query (textToBS)

import Cloud.AWS.ELB.Internal
import Cloud.AWS.ELB.LoadBalancer

initialELBContext :: HTTP.Manager -> AWSContext
initialELBContext mgr = AWSContext
    { manager = mgr
    , endpoint = "elasticloadbalancing.amazonaws.com"
    , lastRequestId = Nothing
    }

runELB :: MonadIO m => ELB m a -> m a
runELB = runAWS initialELBContext

runELBwithManager :: Monad m
    => HTTP.Manager -> AWSSettings -> ELB m a -> m a
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
