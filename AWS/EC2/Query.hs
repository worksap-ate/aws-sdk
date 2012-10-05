{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module AWS.EC2.Query
    ( ec2Query
    , ec2QuerySource
    , ec2QuerySource'
    , QueryParam(..)
    , Filter
    ) where

import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy.Char8 ()

import Data.XML.Types (Event(..))
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Text.XML.Stream.Parse as XmlP
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader
import Control.Exception.Lifted as E
import Data.Text (Text)
import Control.Applicative

import AWS.EC2.Internal
import AWS.EC2.Parser
import AWS.Lib.Query

{- Debug
import Debug.Trace
import qualified Data.Conduit.Binary as CB
import System.IO (stdout)
--}

ec2Version :: ByteString
ec2Version = "2012-08-15"

sinkRequestId :: MonadThrow m
    => GLSink Event m Text
sinkRequestId = do
    await -- EventBeginDocument
    await -- EventBeginElement DescribeImagesResponse
    getT "requestId"

sinkError :: MonadThrow m => Int -> GLSink Event m a
sinkError _ = do
    await
    element "Response" $ do
        (c,m) <- element "Errors" $ element "Error" $
            (,) <$> getT "Code" <*> getT "Message"
        r <- getT "RequestID"
        lift $ monadThrow $ ClientError c m r

ec2Query
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> [QueryParam]
    -> GLSink Event m o
    -> EC2 m o
ec2Query action params sink = do
    src <- ec2QuerySource action params $ sink >>= yield
    lift (src $$ CL.head) >>= maybe (fail "parse error") return

ec2QuerySource
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> [QueryParam]
    -> Conduit Event m o
    -> EC2 m (Source m o)
ec2QuerySource action params cond = do
    ec2QuerySource' action params Nothing cond

ec2QuerySource'
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> [QueryParam]
    -> Maybe Text
    -> Conduit Event m o
    -> EC2 m (Source m o)
ec2QuerySource' action params token cond = do
    cred <- Reader.ask
    ctx <- State.get
    (src1, rid) <- lift $ do
        response <- requestQuery cred ctx action params' ec2Version sinkError
        (res, _) <- unwrapResumable response
--        res $$ CB.sinkFile "debug.txt" >>= fail "debug"
        res $= XmlP.parseBytes XmlP.def $$+ sinkRequestId
    State.put ctx{lastRequestId = Just rid}
    lift $ do
        (src2, _) <- unwrapResumable src1
        return $ src2 $= (cond >> nextToken)
  where
    params' = maybe params
        (\t -> ValueParam "NextToken" t:params) token

    nextToken
        :: (MonadResource m, MonadBaseControl IO m)
        => Conduit Event m o
    nextToken = do
        mt <- getMT "nextToken"
        case mt of
            Nothing -> return ()
            Just t  -> E.throw $ NextToken t
