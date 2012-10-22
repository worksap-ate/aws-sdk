{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Util
    ( asList
    , head
    , each
    , wait
    ) where

import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Class (lift)
import Prelude hiding (head)
import Safe
import qualified Control.Concurrent as CC
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative

import AWS.EC2.Internal

asList
    :: Monad m
    => EC2 m (Source m a)
    -> EC2 m [a]
asList src = do
    s <- src
    lift $ s $$ CL.consume

head
    :: Monad m
    => EC2 m (Source m a)
    -> EC2 m (Maybe a)
head src = do
    s <- src
    lift $ s $$ CL.head

each
    :: Monad m
    => EC2 m (Source m a)
    -> (a -> m b)
    -> EC2 m ()
each src f = do
    s <- src
    (rsrc, _) <- lift $ s $$+ CL.take 0
    lift $ each' rsrc f

each'
    :: Monad m
    => ResumableSource m a
    -> (a -> m b)
    -> m ()
each' rsrc f = do
    (s', ma) <- rsrc $$++ CL.head
    case ma of
        Nothing -> return ()
        Just a  -> f a >> each' s' f

-- | Wait for condition.
--
-- > import AWS.EC2
-- > import AWS.EC2.Types
-- > import AWS.EC2.Util (asList, wait)
-- > 
-- > waitForAvailable :: (MonadIO m, Functor m)
-- >     => Text -- ^ ImageId
-- >     -> EC2 m a
-- > waitForAvailable = wait
-- >     (\img -> imageImageState img == ImageAvailable)
-- >     (\imgId -> asList (describeImages [imgId] [] [] []))
wait
    :: (MonadIO m, Functor m)
    => (a -> Bool) -- ^ condition
    -> (Text -> EC2 m [a]) -- ^ DescribeResources
    -> Text -- ^ Resource Id
    -> EC2 m a
wait f g rid = do
    mr <- headMay <$> g rid
    case mr of
        Nothing -> fail $ "Resource not found: " ++ T.unpack rid
        Just r  -> if f r
            then return r
            else do
                liftIO $ CC.threadDelay 5
                wait f g rid
