module AWS.Util where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Time (UTCTime)
import qualified Data.Time as Time
import System.Locale (defaultTimeLocale)

toS :: BSL.ByteString -> ByteString
toS = BS.concat . BSL.toChunks

toL :: ByteString -> BSL.ByteString
toL = BSL.fromChunks . (:[])

bsShow :: Int -> ByteString
bsShow = BSC.pack . show

err :: String -> Text -> a
err v m = error $ "unknown " ++ v ++ ": " ++ T.unpack m

t2bool :: Text -> Bool
t2bool a
    | a == "true"  = True
    | a == "false" = False
    | otherwise    = err "value" a

t2dec :: Integral a => Text -> a
t2dec t = either 
    (const $ error "not decimal")
    fst
    (TR.decimal t)

t2time :: Text -> UTCTime
t2time = Time.readTime defaultTimeLocale fmt . T.unpack
  where
    fmt = "%FT%T.000Z"

t2emptxt :: Maybe Text -> Text
t2emptxt = maybe "" id

boolToText :: Bool -> Text
boolToText True  = "true"
boolToText False = "false"
