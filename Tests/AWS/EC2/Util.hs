module AWS.EC2.Util
    ( testEC2
    , testEC2'
    )
    where

import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Class (lift)

import AWS
import AWS.EC2

testEC2 region request = do
    cred <- loadCredential
    runResourceT $ do
        runEC2 cred $ do
            setRegion region
            response <- request
            lift $ response $$+- CL.consume

testEC2' region request = do
    cred <- loadCredential
    runResourceT $ do
        runEC2 cred $ do
            setRegion region
            request
