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
import qualified Text.XML.Stream.Parse as XmlP
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)
import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader
import Control.Exception.Lifted as E
import Data.Text (Text)
import Data.Monoid ((<>))
import Control.Applicative

import Cloud.AWS.Lib.Parser.Unordered ((.<), XmlElement, end, (.=), tag, elementConduit, ElementPath, tryConvert, element, elementConsumer, content)

import Cloud.AWS.Class
import Cloud.AWS.EC2.Internal
import Cloud.AWS.Lib.Query
import Cloud.AWS.Lib.ToText (toText)

-- | Ver.2012-12-01
apiVersion :: ByteString
apiVersion = "2012-12-01"

sinkError :: (MonadThrow m, Applicative m)
    => ByteString -> ByteString -> Int -> Consumer Event m a
sinkError ep a s = elementConsumer >>= element "Response" conv
  where
    conv e = do
        (c, m) <- element "Errors" conv' e
        r <- e .< "RequestID"
        monadThrow $ errorData ep a s c m r
    conv' = element "Error" conv''
    conv'' e = (,)
        <$> e .< "Code"
        <*> e .< "Message"
    errorData = if s < 500 then ClientError else ServerError

ec2Query
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> [QueryParam]
    -> (XmlElement -> m o)
    -> EC2 m o
ec2Query action params conv = do
    settings <- Reader.ask
    ctx <- State.get
    e <- lift $ E.handle exceptionTransform $ do
        response <- requestQuery settings ctx action params apiVersion sinkError
        let res = response $=+ XmlP.parseBytes XmlP.def
        res $$+- elementConsumer
    (o, rid) <- lift $ element (toText action <> "Response") conv' e
    State.put ctx{lastRequestId = rid}
    return o
  where
    conv' e = do
        rid <- e .< "requestId"
        o <- conv e
        e .< "nextToken" >>= maybe (return ()) (E.throw . NextToken)
        return (o, rid)

ec2QuerySource
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> [QueryParam]
    -> ElementPath
    -> Conduit XmlElement m o
    -> EC2 m (ResumableSource m o)
ec2QuerySource action params path cond =
    ec2QuerySource' action params Nothing path cond

ec2QuerySource'
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString
    -> [QueryParam]
    -> Maybe Text
    -> ElementPath
    -> Conduit XmlElement m o
    -> EC2 m (ResumableSource m o)
ec2QuerySource' action params token path cond = do
    settings <- Reader.ask
    ctx <- State.get
    src <- lift $ E.handle exceptionTransform $ do
        response <- requestQuery settings ctx action params' apiVersion sinkError
        return $ response
            $=+ XmlP.parseBytes XmlP.def
            $=+ elementConduit path'
    (src', rid) <- lift $ src $$++ sinkReqId
    State.put ctx{lastRequestId = rid}
    return $ src' $=+ (cond >> nextToken)
  where
    params' = ("NextToken" |=? token) : params
    path' = tag (action <> "Response") .= [ end "requestId", end "nextToken", path ]
    sinkReqId = tryConvert (.< "requestId")
    nextToken = tryConvert content >>= maybe (return ()) (E.throw . NextToken)

ec2Delete
    :: (MonadResource m, MonadBaseControl IO m)
    => ByteString -- ^ Name of API
    -> Text -- ^ Parameter Name of ID
    -> Text -- ^ ID of Target
    -> EC2 m Bool
ec2Delete apiName idName targetId = do
    ec2Query apiName [ idName |= targetId ] (.< "return")
