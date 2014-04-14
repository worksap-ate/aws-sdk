{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Cloud.AWS.EC2.Util
    ( list
    , head
    , each
    , eachp
    , wait
    , count
    , findTag
    , sleep
    , retry
    ) where

import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Class (MonadTrans, lift)
import Prelude hiding (head)
import Safe
import qualified Control.Concurrent as CC
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Resource (MonadResource, MonadBaseControl)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative
import Control.Parallel (par)
import Data.List (find)
import qualified Control.Exception.Lifted as E

import Cloud.AWS.EC2.Internal
import Cloud.AWS.EC2.Types (ResourceTag(resourceTagKey))

list
    :: Monad m
    => EC2 m (ResumableSource m a)
    -> EC2 m [a]
list src = do
    s <- src
    lift $ s $$+- CL.consume

head
    :: Monad m
    => EC2 m (ResumableSource m a)
    -> EC2 m (Maybe a)
head src = do
    s <- src
    lift $ s $$+- CL.head

each
    :: Monad m
    => (a -> m b)
    -> EC2 m (ResumableSource m a)
    -> EC2 m ()
each f res = res >>= lift . each' f
  where
    each' g rsrc = do
        (s', ma) <- rsrc $$++ CL.head
        maybe (return ()) (\a -> g a >> each' g s') ma

-- | parallel each
eachp
    :: Monad m
    => (a -> m b)
    -> EC2 m (ResumableSource m a)
    -> EC2 m ()
eachp f res = res >>= lift . each' f
  where
    each' g rsrc = do
        (s', ma) <- rsrc $$++ CL.head
        maybe (return ()) (\a -> g a `par` each' g s') ma

-- | Count resources.
count
    :: Monad m
    => EC2 m (ResumableSource m a)
    -> EC2 m Int
count ers = do
    s <- ers
    lift $ s $$+- c 0
  where
    c n = await >>= maybe (return n) (const $ c $ n + 1)

-- | Wait for condition.
--
-- > import Cloud.AWS.EC2
-- > import Cloud.AWS.EC2.Types
-- > import Cloud.AWS.EC2.Util (list, wait)
-- > 
-- > waitForAvailable :: (MonadIO m, Functor m)
-- >     => Text -- ^ ImageId
-- >     -> EC2 m a
-- > waitForAvailable = wait
-- >     (\img -> imageImageState img == ImageAvailable)
-- >     (\imgId -> list (describeImages [imgId] [] [] []))
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
                sleep 5
                wait f g rid

findTag
    :: Text -- ^ resourceKey
    -> [ResourceTag] -- ^ TagSet
    -> Maybe ResourceTag
findTag key tags = find f tags
  where
    f t = resourceTagKey t == key

sleep :: MonadIO m => Int -> EC2 m ()
sleep sec = liftIO $ CC.threadDelay $ sec * 1000 * 1000

retry
    :: forall m a. (MonadBaseControl IO m, MonadResource m)
    => Int -- ^ sleep count
    -> Int -- ^ number of retry
    -> EC2 m a
    -> EC2 m a
retry _   0   f = f
retry sec cnt f = f `E.catch` handler
  where
    handler :: E.SomeException -> EC2 m a
    handler _ = do
        sleep sec
        retry sec (cnt - 1) f
