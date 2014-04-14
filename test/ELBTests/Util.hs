module ELBTests.Util
    ( testELB
    )
    where

import Data.Text (Text)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)

import Cloud.AWS.ELB

testELB
    :: Text
    -> ELB (ResourceT IO) a
    -> IO a
testELB region request = do
    runResourceT $ runELB $ do
        setRegion region
        request
