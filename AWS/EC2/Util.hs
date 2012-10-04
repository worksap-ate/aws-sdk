{-# LANGUAGE FlexibleContexts #-}

module AWS.EC2.Util
    ( asList
    , head
    , each
    ) where

import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Class (lift)
import Prelude hiding (head)

import AWS.EC2.Class

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
