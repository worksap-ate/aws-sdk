{-# LANGUAGE FlexibleContexts, RankNTypes, CPP #-}

module AWS.RDS.Internal
    where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Conduit
import Data.Monoid ((<>))
import Data.XML.Types (Event(..))

import AWS.Class
import AWS.Lib.Query
import AWS.Lib.Parser

apiVersion :: ByteString
apiVersion = "2012-09-17"

type RDS m a = AWS AWSContext m a

rdsQuery
    :: (MonadBaseControl IO m, MonadResource m)
    => ByteString -- ^ Action
    -> [QueryParam]
    -> GLSink Event m a
    -> RDS m a
rdsQuery = commonQuery apiVersion

elements :: MonadThrow m
    => Text
    -> GLSink Event m a
    -> GLSink Event m [a]
elements name f = element (name <> "s") $ listConsumer name f

#ifdef DEBUG
rdsQueryDebug
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> [QueryParam]
    -> RDS m a
rdsQueryDebug = debugQuery apiVersion
#endif
