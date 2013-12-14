{-# LANGUAGE FlexibleContexts, RankNTypes, RecordWildCards #-}

module Cloud.AWS.CloudWatch.Internal
    where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Conduit

import Cloud.AWS.Class
import Cloud.AWS.Lib.Query
import Cloud.AWS.Lib.Parser.Unordered (SimpleXML, (.<))
import Cloud.AWS.CloudWatch.Types

-- | Ver.2010-08-01
apiVersion :: ByteString
apiVersion = "2010-08-01"

type CloudWatch m a = AWS AWSContext m a

cloudWatchQuery
    :: (MonadBaseControl IO m, MonadResource m)
    => ByteString -- ^ Action
    -> [QueryParam]
    -> (SimpleXML -> m a)
    -> CloudWatch m a
cloudWatchQuery = commonQuery apiVersion

sinkDimension :: (MonadThrow m, Applicative m)
    => SimpleXML -> m Dimension
sinkDimension xml = Dimension <$> xml .< "Name" <*> xml .< "Value"

fromDimension :: Dimension -> [QueryParam]
fromDimension Dimension{..} =
    [ "Name" |= dimensionName
    , "Value" |= dimensionValue
    ]
