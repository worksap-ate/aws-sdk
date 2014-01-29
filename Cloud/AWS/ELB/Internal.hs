{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Cloud.AWS.ELB.Internal
    where

import Data.ByteString (ByteString)
import Data.Conduit

import Cloud.AWS.Class
import Cloud.AWS.Lib.Query
import Cloud.AWS.Lib.Parser.Unordered (XmlElement)

-- | Ver.2012-06-01
apiVersion :: ByteString
apiVersion = "2012-06-01"

type ELB m a = AWS AWSContext m a

elbQuery
    :: (MonadBaseControl IO m, MonadResource m)
    => ByteString -- ^ Action
    -> [QueryParam]
    -> (XmlElement -> m a)
    -> AWS AWSContext m a
elbQuery = commonQuery apiVersion
