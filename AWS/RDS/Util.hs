module AWS.RDS.Util
    ( wait
    ) where

import Control.Applicative ((<$>))
import qualified Control.Concurrent as CC
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Text (Text)
import qualified Data.Text as T
import Safe

import AWS.RDS

wait
    :: (MonadIO m, Functor m)
    => (a -> Bool) -- ^ condition
    -> (Text -> RDS m [a]) -- ^ DescribeResources
    -> Text -- ^ Resource Id
    -> RDS m a
wait f g rid = do
    mr <- headMay <$> g rid
    case mr of
        Nothing -> fail $ "Resource not found: " ++ T.unpack rid
        Just r  -> if f r
            then return r
            else do
                liftIO $ CC.threadDelay 10000000
                wait f g rid
