-- |  aws-sdk is an AWS library for Haskell
--
-- Put your AWS AccessKey and SecretAccessKey into a configuration
-- file. Write the following in /./\//aws.config/.
--
-- > accessKey: your-access-key
-- > secretAccessKey: your-secret-access-key
--
-- The following is quick example(DescribeInstances).
--
-- > module Example where
-- > 
-- > import Data.Conduit
-- > import qualified Data.Conduit.List as CL
-- > import Control.Monad.IO.Class (liftIO)
-- > import Control.Monad.Trans.Class (lift)
-- > 
-- > import Cloud.AWS
-- > import Cloud.AWS.EC2
-- > import qualified Cloud.AWS.EC2.Util as Util
-- > 
-- > main :: IO ()
-- > main = do
-- >     doc <- runResourceT $ runEC2 $
-- >         Util.list $ describeInstances [] []
-- >     print doc
-- >     putStr "Length: "
-- >     print $ length doc
{-# LANGUAGE OverloadedStrings #-}
module Cloud.AWS
    ( -- * Credentials
      Credential
    , AccessKey
    , SecretAccessKey
    , newCredential
    , loadCredential
    , loadCredentialFromFile
    , defaultSettings
      -- * Environment
    , AWS
    , AWSException(..)
    , getLastRequestId
    ) where

import Cloud.AWS.Credential
import Cloud.AWS.Class
