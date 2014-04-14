{-# LANGUAGE FlexibleContexts, RankNTypes, CPP #-}

module AWS.EC2.Query
    ( ec2Query
    , ec2QuerySource
    , ec2QuerySource'
    , ec2Delete
    , module AWS.Lib.Query
    , apiVersion
    ) where

import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy.Char8 ()

import Data.XML.Types (Event(..))
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadThrow, MonadBaseControl, MonadResource)
#endif
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Text.XML.Stream.Parse as XmlP
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader
import Control.Exception.Lifted as E
import Data.Text (Text)
import Control.Applicative

import AWS.Class
import AWS.EC2.Internal
import AWS.Lib.Parser hiding (sinkError)
import AWS.Lib.Query

-- | Ver.2012-12-01
apiVersion :: ByteString
apiVersion = "2012-12-01"

sinkRequestId :: MonadThrow m
    => Consumer Event m (Maybe Text)
sinkRequestId = do
    await -- EventBeginDocument
    await -- EventBeginElement DescribeImagesResponse
    getT "requestId"

sinkError :: MonadThrow m => ByteString -> Int -> Consumer Event m a
sinkError a s = do
    await
    element "Response" $ do
        (c,m) <- element "Errors" $ element "Error" $
            (,) <$> getT "Code" <*> getT "Message"
        r <- getT "RequestID"
        lift $ monadThrow $ ClientError a s c m r

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
        response
            $=+ XmlP.parseBytes XmlP.def
            $$++ sinkRequestId
    State.put ctx{lastRequestId = rid}
    return $ src1 $=+ (cond >> nextToken)
  where
    params' = ("NextToken" |=? token) : params

nextToken :: MonadThrow m => Conduit Event m o
nextToken = getT "nextToken" >>= maybe (return ()) (E.throw . NextToken)

ec2Delete
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString -- ^ Name of API
    -> Text -- ^ Parameter Name of ID
    -> Text -- ^ ID of Target
    -> EC2 m Bool
ec2Delete apiName idName targetId = do
    ec2Query apiName [ idName |= targetId ] $ getT "return"
