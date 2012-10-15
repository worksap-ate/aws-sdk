{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.ELB.Internal
    where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Conduit
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader
import qualified Text.XML.Stream.Parse as XML
import Data.XML.Types (Event(..))

import AWS.Class
import AWS.Util
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
    -> ELB m a
elbQuery action params sink = do
    ctx <- State.get
    cred <- Reader.ask
    rs <- lift $ requestQuery cred ctx action params apiVersion undefined
--    lift $ rs $$+- CB.sinkFile "debug.txt" >> fail "debug"
    (res, rid) <- lift $ rs $$+-
        XML.parseBytes XML.def =$ sinkResponse (bsToText action) sink
    State.put ctx { lastRequestId = Just rid }
    return res

members :: MonadThrow m
    => Text
    -> GLSink Event m a
    -> GLSink Event m [a]
members name f = element name $ listConsumer "member" f
