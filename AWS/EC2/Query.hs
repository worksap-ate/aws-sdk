{-# LANGUAGE FlexibleContexts, RankNTypes, CPP #-}

module AWS.EC2.Query
    ( ec2Query
    , ec2QuerySource
    , ec2QuerySource'
#ifdef DEBUG
    , ec2QueryDebug
#endif
    , ec2Delete
    , module AWS.Lib.Query
    ) where

import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy.Char8 ()

import Data.XML.Types (Event(..))
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Text.XML.Stream.Parse as XmlP
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader
import Control.Exception.Lifted as E
import Data.Text (Text)
import Control.Applicative
import Data.Conduit.Internal as CI

import AWS.Class
import AWS.EC2.Internal
import AWS.Lib.Parser hiding (sinkError)
import AWS.Lib.Query

#ifdef DEBUG
import Debug.Trace
import qualified Data.Conduit.Binary as CB
#endif

ec2Version :: ByteString
ec2Version = "2012-10-01"

sinkRequestId :: MonadThrow m
    => GLSink Event m Text
sinkRequestId = do
    await -- EventBeginDocument
    await -- EventBeginElement DescribeImagesResponse
    getT "requestId"

sinkError :: MonadThrow m => Int -> GLSink Event m a
sinkError s = do
    await
    element "Response" $ do
        (c,m) <- element "Errors" $ element "Error" $
            (,) <$> getT "Code" <*> getT "Message"
        r <- getT "RequestID"
        lift $ monadThrow $ ClientError s c m r

ec2Query
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> [QueryParam]
    -> GLSink Event m o
    -> EC2 m o
ec2Query action params sink = do
    src <- ec2QuerySource action params $ sink >>= yield
    lift (src $$+- CL.head) >>= maybe (fail "parse error") return

ec2QuerySource
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> [QueryParam]
    -> Conduit Event m o
    -> EC2 m (ResumableSource m o)
ec2QuerySource action params cond = do
    ec2QuerySource' action params Nothing cond

($=+) :: MonadIO m
    => ResumableSource m a
    -> Conduit a m b
    -> m (ResumableSource m b)
a $=+ b = do
    (sa, fa) <- unwrapResumable a
    return $ CI.ResumableSource (sa $= b) fa

ec2QuerySource'
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> [QueryParam]
    -> Maybe Text
    -> Conduit Event m o
    -> EC2 m (ResumableSource m o)
ec2QuerySource' action params token cond = do
    cred <- Reader.ask
    ctx <- State.get
    (src1, rid) <- lift $ do
        response <- requestQuery cred ctx action params' ec2Version sinkError
        res <- response $=+ XmlP.parseBytes XmlP.def
        res $$++ sinkRequestId
    State.put ctx{lastRequestId = Just rid}
    lift $ src1 $=+ (cond >> nextToken)
  where
    params' = maybe params
        (\t -> ValueParam "NextToken" t:params) token

nextToken :: MonadThrow m => Conduit Event m o
nextToken = getMT "nextToken" >>= maybe (return ()) (E.throw . NextToken)

#ifdef DEBUG
ec2QueryDebug
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> [QueryParam]
    -> EC2 m (Source m o)
ec2QueryDebug action params = do
    cred <- Reader.ask
    ctx <- State.get
    lift $ do
        response <- requestQuery cred ctx action params ec2Version sinkError
        (res, _) <- unwrapResumable response
        res $$ CB.sinkFile "debug.txt" >>= fail "debug"
#endif

ec2Delete
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString -- ^ Name of API
    -> Text -- ^ Parameter Name of ID
    -> Text -- ^ ID of Target
    -> EC2 m Bool
ec2Delete apiName idName targetId = do
    ec2Query apiName [ ValueParam idName targetId ] returnBool
