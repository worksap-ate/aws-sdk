module AWS.Util where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL

toS :: BSL.ByteString -> ByteString
toS = BS.concat . BSL.toChunks

toL :: ByteString -> BSL.ByteString
toL = BSL.fromChunks . (:[])

bsShow :: Int -> ByteString
bsShow = BSC.pack . show
