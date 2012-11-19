{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.ELB.Internal
    where

import Data.ByteString (ByteString)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.XML.Types (Event(..))

import AWS.Class
import AWS.Lib.Query

apiVersion :: ByteString
apiVersion = "2012-06-01"

type ELB m a = AWS AWSContext m a

elbQuery
    :: (MonadBaseControl IO m, MonadResource m)
    => ByteString -- ^ Action
    -> [QueryParam]
    -> GLSink Event m a
    -> AWS AWSContext m a
elbQuery = commonQuery apiVersion
