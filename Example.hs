module Example where

import Data.Conduit
import qualified Data.Conduit.List as CL
import Network.HTTP.Conduit (def, newManager, parseUrl)
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime)

import AWS
import AWS.EC2

main :: IO ()
main = do
    cred <- loadCredential
    time <- getCurrentTime
    let url = mkUrl UsEast1 cred time
    doc <- runResourceT $ do
        manager <- liftIO $ newManager def
        request <- liftIO $ parseUrl url
        response <- describeImages request manager
        imagesSet response $$ CL.consume
    print doc
    putStr "Length: "
    print $ length doc
