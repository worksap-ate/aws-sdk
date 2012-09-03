{-# LANGUAGE OverloadedStrings #-}
module AWS where

import Data.Digest.Pure.SHA

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Char8 as BC
import Data.Word

import qualified Codec.Binary.Base64 as Base64
import qualified Codec.Binary.Base64Url as Base64Url
import qualified Codec.Binary.Url as Url

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Network.HTTP.Conduit
import Control.Monad.IO.Class (liftIO)
import System.IO

import Data.Time
import System.Locale
import Data.List

data Credential = Credential {
        accessKey :: ByteString,
        secretAccessKey :: ByteString
    } deriving (Show)

loadCredential :: IO Credential
loadCredential = withFile (LC.unpack "aws.config") ReadMode $ \h -> do
    a <- BC.hGetLine h
    s <- BC.hGetLine h
    return $ Credential {
        accessKey = LC.pack $ BC.unpack a,
        secretAccessKey = LC.pack $ BC.unpack s}

endpointuse :: ByteString
endpointuse = "ec2.amazonaws.com"

type Param = (ByteString, ByteString)

wrap :: ([Word8] -> String) -> ByteString -> ByteString
wrap f = LC.pack . f . L.unpack

params :: ByteString -> ByteString -> [Param]
params a t =
    [("Action", "DescribeImages"),
     ("ImageId.1", "ami-e565ba8c"),
     ("Timestamp", wrap Url.encode t),
     ("Version", "2012-07-20"),
--     ("Expires", wrap Url.encode "2008-02-10T12:00:00Z"),
     ("SignatureVersion", "2"),
     ("SignatureMethod", "HmacSHA256"),
     ("AWSAccessKeyId", a)]

queryStr :: [Param] -> ByteString
queryStr = join "&" . map param
  where
    param :: Param -> ByteString
    param (k, v) = L.concat [k, "=", v]

    join :: ByteString -> [ByteString] -> ByteString
    join sep = foldl1 $ \a b -> L.concat [a, sep, b]

signature :: ByteString -> ByteString -> ByteString -> ByteString
signature endpoint secret query =
    wrap Url.encode base64string
  where
    stringToSign = L.concat ["GET\n", endpoint, "\n/\n", query]
    signedString = hmacSha256 secret stringToSign
    base64urlstring = wrap Base64Url.encode $ bytestringDigest signedString
    base64string = wrap Base64.encode $ bytestringDigest signedString

main :: IO ()
main = do
    let fmt = iso8601DateFormat (Just "%XZ")
    time <- getCurrentTime
    cred <- loadCredential
    let t = LC.pack $ formatTime defaultTimeLocale fmt time
    let tmp = queryStr $ sort $ params (accessKey cred) t
    let s = signature endpointuse (secretAccessKey cred) tmp
    let query = L.concat [tmp, "&Signature=", s]
    LC.putStrLn query
    runResourceT $ do
        manager <- liftIO $ newManager def
        let url = L.concat ["https://", endpointuse, "/?", query]
        liftIO $ LC.putStrLn url
        request <- liftIO $ parseUrl $ LC.unpack url
        response <- http request manager
        responseBody response $$+- CB.sinkHandle stdout

