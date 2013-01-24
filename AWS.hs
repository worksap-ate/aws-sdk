-- | aws-sdk is an AWS library for Haskell
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
-- > import AWS
-- > import AWS.EC2
-- > import qualified AWS.EC2.Util as Util
-- > 
-- > main :: IO ()
-- > main = do
-- >     cred <- loadCredential
-- >     doc <- runResourceT $
-- >         runEC2 cred $
-- >             Util.list $ describeInstances [] []
-- >     print doc
-- >     putStr "Length: "
-- >     print $ length doc
{-# LANGUAGE OverloadedStrings #-}
module AWS
    ( -- * Credentials
      Credential
    , AccessKey
    , SecretAccessKey
    , newCredential
    , loadCredential
    , loadCredentialFromFile
      -- * Environment
    , AWS
    , AWSException(..)
    , getLastRequestId
    ) where

import AWS.Credential
import AWS.Class
