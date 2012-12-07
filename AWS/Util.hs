module AWS.Util where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Time (UTCTime)
import qualified Data.Time as Time
import qualified Data.Time.Parse as TP
import System.Locale (defaultTimeLocale)

toS :: BSL.ByteString -> ByteString
toS = BS.concat . BSL.toChunks

toL :: ByteString -> BSL.ByteString
toL = BSL.fromChunks . (:[])

bsShow :: Show a => a -> ByteString
bsShow = BSC.pack . show

err :: String -> Text -> a
err v m = error $ "unknown " ++ v ++ ": " ++ T.unpack m

textToBool :: Text -> Bool
textToBool a
    | a == "true"  = True
    | a == "false" = False
    | otherwise    = err "value" a

textToInt :: Integral a => Text -> a
textToInt t = either 
    (const $ error "not decimal")
    fst
    (TR.signed TR.decimal t)

textToDouble :: Fractional a => Text -> a
textToDouble t = either
    (const $ error "not double value")
    fst
    (TR.signed TR.rational t)

textToTime :: Text -> UTCTime
textToTime
    = Time.localTimeToUTC Time.utc
    . maybe (error "time format error.") fst
    . TP.strptime fmt
    . T.unpack
  where
    fmt = "%FT%T"

timeToText :: UTCTime -> Text
timeToText
    = T.pack
    . Time.formatTime defaultTimeLocale fmt
  where
    fmt = "%FT%T"

orEmpty :: Monoid a => Maybe a -> a
orEmpty = fromMaybe mempty

boolToText :: Bool -> Text
boolToText True  = "true"
boolToText False = "false"

toText :: Show a => a -> Text
toText = T.pack . show

bsToText :: ByteString -> Text
bsToText = T.pack . BSC.unpack

textToBS :: Text -> ByteString
textToBS = BSC.pack . T.unpack

unconcat :: [a] -> [[a]]
unconcat = map (:[])
