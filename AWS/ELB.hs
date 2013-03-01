{-# LANGUAGE FlexibleContexts, RankNTypes #-}

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
import Data.Conduit
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.State as State
import qualified Network.HTTP.Conduit as HTTP
import Data.Monoid

import AWS.Class
import AWS.Lib.Query (textToBS)

import AWS.ELB.Internal
import AWS.ELB.LoadBalancer

initialELBContext :: HTTP.Manager -> AWSContext
initialELBContext mgr = AWSContext
    { manager = mgr
    , endpoint = "elasticloadbalancing.amazonaws.com"
    , lastRequestId = Nothing
    }

runELB :: MonadIO m => AWSSettings -> ELB m a -> m a
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
