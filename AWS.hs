-- | aws-sdk is an AWS library for Haskell
--
-- Put your AWS AccessKey and SecretAccessKey into a configuration
-- file. Write the following in /./\//aws.config/.
--
-- > accessKey: your-access-key
-- > secretAccessKey: your-secret-access-key
{-# LANGUAGE OverloadedStrings #-}
module AWS
    ( -- * Credentials
      Credential
    , loadCredential
    ) where

import AWS.Credential
