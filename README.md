[![Build Status](https://secure.travis-ci.org/worksap-ate/aws-sdk.png)](http://travis-ci.org/worksap-ate/aws-sdk)

# AWS-SDK

An AWS(Amazon Web Services) library for Haskell.

## Usage

Put your AWS AccessKey and SecretAccessKey into a configuration file.
Write the followfing in `$aws-sdk/aws.config`.

    accessKey: your-access-key
    secretAccessKey: your-secret-access-key

The following is quick example(DescribeInstances).

~~~~~~~~ {.haskell}
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
~~~~~~
