{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module AWS.EC2
    ( module AWS.EC2.Types
    , module AWS.EC2.Class
    , EC2Endpoint(..)
    , newEC2Context
    , setEndpoint
    , module AWS.EC2.Image
    , module AWS.EC2.Region
    , module AWS.EC2.AvailabilityZone
    , module AWS.EC2.Instance
    , module AWS.EC2.Address
    , module AWS.EC2.Tag
    ) where

import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Monad.State as ST
import qualified Network.HTTP.Conduit as HTTP

import AWS.Types
import AWS.EC2.Types
import AWS.EC2.Class

import AWS.EC2.Image
import AWS.EC2.Region
import AWS.EC2.AvailabilityZone
import AWS.EC2.Instance
import AWS.EC2.Address
import AWS.EC2.Tag

newEC2Context :: Credential -> IO EC2Context
newEC2Context cred = do
    mgr <- HTTP.newManager HTTP.def
    return EC2Context
        { manager = mgr
        , credential = cred
        , endpoint = UsEast1
        , lastRequestId = Nothing
        }

setEndpoint
    :: (MonadResource m, MonadBaseControl IO m)
    => EC2Endpoint -> EC2 m ()
setEndpoint ep = do
    ctx <- ST.get
    ST.put ctx { endpoint = ep }
