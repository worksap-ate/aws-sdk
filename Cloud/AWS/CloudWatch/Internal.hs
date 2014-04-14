{-# LANGUAGE FlexibleContexts, RankNTypes, RecordWildCards #-}

module Cloud.AWS.CloudWatch.Internal
    where

import Cloud.AWS.Lib.Parser.Unordered (XmlElement, (.<))
import Control.Applicative
import Control.Monad.Trans.Resource (MonadThrow, MonadResource, MonadBaseControl)
import Data.ByteString (ByteString)

import Cloud.AWS.Class
import Cloud.AWS.Lib.Query
import Cloud.AWS.CloudWatch.Types

-- | Ver.2010-08-01
apiVersion :: ByteString
apiVersion = "2010-08-01"

type CloudWatch m a = AWS AWSContext m a

cloudWatchQuery
    :: (MonadBaseControl IO m, MonadResource m)
    => ByteString -- ^ Action
    -> [QueryParam]
    -> (XmlElement -> m a)
    -> CloudWatch m a
cloudWatchQuery = commonQuery apiVersion

sinkDimension :: (MonadThrow m, Applicative m)
    => XmlElement -> m Dimension
sinkDimension xml = Dimension <$> xml .< "Name" <*> xml .< "Value"

fromDimension :: Dimension -> [QueryParam]
fromDimension Dimension{..} =
    [ "Name" |= dimensionName
    , "Value" |= dimensionValue
    ]
