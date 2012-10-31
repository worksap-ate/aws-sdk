{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module AWS.Credential
    ( Credential(..)
    , AccessKey
    , SecretAccessKey
    , loadCredential
    , loadCredentialFromFile
    , newCredential
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

-- | Load credential from \"./aws.config\".
loadCredential :: IO Credential
loadCredential = loadCredentialFromFile "aws.config"

-- | Create new credential.
newCredential :: AccessKey -> SecretAccessKey -> Credential
newCredential key secret = Credential key secret

-- | Load credential from file.
loadCredentialFromFile :: FilePath -> IO Credential
loadCredentialFromFile path = do
    str <- BS.readFile path
    case parse configParser "" str of
        Left err   -> fail $ show err
        Right conf -> return conf
