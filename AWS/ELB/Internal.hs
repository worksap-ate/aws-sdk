{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.ELB.Internal
    where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.XML.Types (Event(..))

import AWS.Class
import AWS.Lib.Query
import AWS.Lib.Parser

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

members :: MonadThrow m
    => Text
    -> GLSink Event m a
    -> GLSink Event m [a]
members name f = element name $ listConsumer "member" f
