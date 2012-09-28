module Example where

import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

import AWS
import AWS.EC2

main :: IO ()
main = do
    cred <- loadCredential
    doc <- runResourceT $ do
        ctx <- liftIO $ newEC2Context cred
        runEC2 ctx $ do
            response <- describeInstances [] []
            lift $ response $$ CL.consume
    print doc
    putStr "Length: "
    print $ length doc
