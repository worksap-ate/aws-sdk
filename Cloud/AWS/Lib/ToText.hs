{-# LANGUAGE FlexibleInstances #-}

module Cloud.AWS.Lib.ToText
    ( ToText (toText)
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IP (IPv4, AddrRange)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.Time as Time
import System.Locale (defaultTimeLocale)

class ToText a where
    toText :: a -> Text
    toMaybeText :: a -> Maybe Text
    toMaybeText = Just . toText

instance ToText Text where
    toText t = t

instance ToText ByteString where
    toText = T.pack . BC.unpack

instance ToText Bool where
    toText True  = "true"
    toText False = "false"

instance ToText UTCTime where
    toText
        = T.pack
        . Time.formatTime defaultTimeLocale "%FT%T"

instance ToText Int where
    toText = toTextS

instance ToText Double where
    toText = toTextS

instance ToText IPv4 where
    toText = toTextS

instance ToText (AddrRange IPv4) where
    toText = toTextS

toTextS :: Show a => a -> Text
toTextS = T.pack . show
