{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.ELB.Internal
    where

import Data.ByteString (ByteString)
import Data.Conduit
import Data.XML.Types (Event(..))

import AWS.Class
import AWS.Lib.Query

-- | Ver.2012-06-01
apiVersion :: ByteString
apiVersion = "2012-06-01"

type ELB m a = AWS AWSContext m a

elbQuery
    :: (MonadBaseControl IO m, MonadResource m)
    => ByteString -- ^ Action
    -> [QueryParam]
    -> Consumer Event m a
    -> AWS AWSContext m a
elbQuery = commonQuery apiVersion
