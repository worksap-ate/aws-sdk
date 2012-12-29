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
    import Control.Monad.IO.Class (liftIO)
    import Control.Monad.Trans.Class (lift)
    
    import AWS
    import AWS.EC2
    import qualified AWS.EC2.Util as Util
    
    main :: IO ()
    main = do
        cred <- loadCredential
        doc <- runResourceT $
            runEC2 cred $
                Util.list $ describeInstances [] []
        print doc
        putStr "Length: "
        print $ length doc
~~~~~~
