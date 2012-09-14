{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module AWS.Credential
    ( Credential(..)
    , AccessKey
    , SecretAccessKey
    , loadCredential
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Text.Config
import Text.Parsec

mkConfig "configParser" [config|
Credential
    accessKey       ByteString
    secretAccessKey ByteString
|]

type AccessKey = ByteString
type SecretAccessKey = ByteString

loadCredential :: IO Credential
loadCredential = do
    str <- BS.readFile "aws.config"
    case parse configParser "" str of
        Left err   -> fail $ show err
        Right conf -> return conf