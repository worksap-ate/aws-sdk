{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Cloud.AWS.EC2.Query
    ( ec2Query
    , ec2QuerySource
    , ec2QuerySource'
    , ec2Delete
    , module Cloud.AWS.Lib.Query
    , apiVersion
    ) where

import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy.Char8 ()

import Data.XML.Types (Event)
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Text.XML.Stream.Parse as XmlP
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader
import Control.Exception.Lifted as E
import Data.Text (Text)
import Control.Applicative

import Cloud.AWS.Class
import Cloud.AWS.EC2.Internal
import Cloud.AWS.Lib.Parser.Unordered (SimpleXML, xmlParser, (.<), getElement)
import Cloud.AWS.Lib.Query

-- | Ver.2012-12-01
apiVersion :: ByteString
apiVersion = "2012-12-01"

sinkRequestId :: MonadThrow m
    => Consumer Event m (Maybe Text)
sinkRequestId = do
    await -- EventBeginDocument
    await -- EventBeginElement DescribeImagesResponse
    xmlParser (.< "requestId")

sinkRequestId' :: (MonadThrow m, Applicative m)
    => SimpleXML -> m (Maybe Text)
sinkRequestId' = (.< "requestId")

sinkError :: (MonadThrow m, Applicative m)
    => ByteString -> ByteString -> Int -> Consumer Event m a
sinkError ep a s = xmlParser $ \xml -> do
    getElement xml "Response" $ \xml1 -> do
        (c, m) <- getElement xml1 "Errors" $ \xml2 ->
            getElement xml2 "Error" $ \xml3 ->
                (,) <$> xml3 .< "Code"
                    <*> xml3 .< "Message"
        r <- xml1 .< "RequestID"
        monadThrow $ errorData ep a s c m r
  where
    errorData = if s < 500 then ClientError else ServerError

ec2Query
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> [QueryParam]
    -> Consumer Event m o
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

ec2QuerySource'
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> [QueryParam]
    -> Maybe Text
    -> Conduit Event m o
    -> EC2 m (ResumableSource m o)
ec2QuerySource' action params token cond = do
    settings <- Reader.ask
    ctx <- State.get
    (src1, rid) <- lift $ E.handle exceptionTransform $ do
        response <- requestQuery settings ctx action params' apiVersion sinkError
        res <- response $=+ XmlP.parseBytes XmlP.def
        res $$++ sinkRequestId
    State.put ctx{lastRequestId = rid}
    lift $ src1 $=+ (cond >> nextToken)
  where
    params' = ("NextToken" |=? token) : params

nextToken :: MonadThrow m => Conduit Event m o
nextToken = xmlParser $ \xml -> (xml .< "nextToken") >>= maybe (return ()) (E.throw . NextToken)

ec2Delete
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString -- ^ Name of API
    -> Text -- ^ Parameter Name of ID
    -> Text -- ^ ID of Target
    -> EC2 m Bool
ec2Delete apiName idName targetId = do
    ec2Query apiName [ idName |= targetId ] $ xmlParser (.< "return")
