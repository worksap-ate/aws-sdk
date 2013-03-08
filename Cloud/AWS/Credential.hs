{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables #-}

module Cloud.AWS.Credential
    ( Credential(..)
    , AccessKey
    , SecretAccessKey
    , loadCredential
    , loadCredentialFromFile
    , newCredential
    ) where

import Control.Exception.IOChoice (runAnyOne)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath ((</>))
import Text.Config
import Text.Parsec (parse)

mkConfig "configParser" [config|
Credential
    accessKey       ByteString
    secretAccessKey ByteString
|]

type AccessKey = ByteString
type SecretAccessKey = ByteString

defaultConfigFile :: FilePath
defaultConfigFile = "aws.config"

-- | Load credential from \".\/aws.config\" or \"~\/aws.config\".
loadCredential :: IO Credential
loadCredential =
    runAnyOne filePaths >>= loadCredentialFromFile
  where
    getPath dir = doesFileExist path >>= iff
        (return path)
        (fail "\"aws.config\" does not exist.")
      where
        path = dir </> defaultConfigFile
        iff t f c = if c then t else f
    filePaths = map (>>= getPath)
        [ getCurrentDirectory
        , getHomeDirectory
        ]

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
