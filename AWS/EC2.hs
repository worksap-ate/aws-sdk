{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module AWS.EC2
    ( module AWS.EC2.Types
    , EC2Endpoint(..)
    , EC2
    , EC2Context
    , Filter
    , newEC2Context
    , runEC2
    , setEndpoint
    , module AWS.EC2.Image
    , module AWS.EC2.Region
    , module AWS.EC2.AvailabilityZone
    , module AWS.EC2.Instance
    , module AWS.EC2.Address
    ) where

import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Monad.State as ST
import qualified Network.HTTP.Conduit as HTTP

import AWS.Types
import AWS.EC2.Types
import AWS.EC2.Query

import AWS.EC2.Image
import AWS.EC2.Region
import AWS.EC2.AvailabilityZone
import AWS.EC2.Instance
import AWS.EC2.Address

newEC2Context :: Credential -> IO EC2Context
newEC2Context cred = do
    mgr <- HTTP.newManager HTTP.def
    return EC2Context
        { manager = mgr
        , credential = cred
        , endpoint = UsEast1
        }

runEC2
    :: (MonadResource m, MonadBaseControl IO m)
    => EC2Context
    -> EC2 m a
    -> m a
runEC2 ctx = flip ST.evalStateT ctx

setEndpoint
    :: (MonadResource m, MonadBaseControl IO m)
    => EC2Endpoint -> EC2 m ()
setEndpoint ep = do
    ctx <- ST.get
    ST.put ctx { endpoint = ep }
