module Example where

import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Class (lift)

import Cloud.AWS.EC2

main :: IO ()
main = do
    doc <- runResourceT $ do
        runEC2 $ do
            setRegion "ap-northeast-1"
            response <- describeInstances [] []
            lift $ response $$+- CL.consume
    print doc
    putStr "Length: "
    print $ length doc
