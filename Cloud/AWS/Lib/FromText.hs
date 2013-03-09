{-# LANGUAGE RankNTypes, OverloadedStrings, TemplateHaskell, FlexibleInstances #-}

module Cloud.AWS.Lib.FromText
    ( FromText(..)
    , deriveFromText
    -- re-exports
    , monadThrow
    , IPv4
    , AddrRange
    , Text
    , UTCTime
    , AWSException(..)
    ) where

import Control.Applicative ((<$>))
import Control.Monad
import Data.Conduit (MonadThrow)
import Data.IP (IPv4, AddrRange)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.Time as Time
import qualified Data.Time.Parse as TP
import Language.Haskell.TH
import Safe

import Cloud.AWS.Class

class FromText a
  where
    fromText :: MonadThrow m => Text -> m a
    fromText t
        = maybe (monadThrow $ FromTextError t) return
        . fromTextMaybe
        $ t

    fromTextMaybe :: Text -> Maybe a

    fromMaybeText :: MonadThrow m => Text -> Maybe Text -> m a
    fromMaybeText name
        = maybe
            (monadThrow $ FromTextError $ "no text: " <> name)
            fromText

instance FromText a => FromText (Maybe a)
  where
    fromText = return . join . fromTextMaybe
    fromMaybeText _name Nothing  = return Nothing
    fromMaybeText _name (Just t) = return $ fromTextMaybe t
    fromTextMaybe = Just . fromTextMaybe

instance FromText Int where
    fromTextMaybe = fromTextMaybeR

instance FromText Integer where
    fromTextMaybe = fromTextMaybeR

instance FromText Double where
    fromTextMaybe = fromTextMaybeR

instance FromText IPv4 where
    fromTextMaybe = fromTextMaybeR

instance FromText (AddrRange IPv4) where
    fromTextMaybe = fromTextMaybeR

fromTextMaybeR :: Read a => Text -> Maybe a
fromTextMaybeR = readMay . T.unpack

instance FromText Text
  where
    fromTextMaybe t
        | t == ""   = Nothing
        | otherwise = Just t

instance FromText Bool
  where
    fromTextMaybe "true"  = Just True
    fromTextMaybe "false" = Just False
    fromTextMaybe _       = Nothing

instance FromText UTCTime
  where
    fromTextMaybe t
        = Time.localTimeToUTC Time.utc . fst
        <$> (TP.strptime fmt $ T.unpack t)
      where
        fmt = "%FT%T"

deriveFromText :: String -> [String] -> DecsQ
deriveFromText dstr strs = do
    ctrs <- map (\(NormalC name _) -> name) <$> cons
    x <- newName "x"
    let cases = caseE (varE x) (map f (zip strs ctrs) ++ [wild])
    let fun = funD 'fromTextMaybe [clause [varP x] (normalB cases) []]
    (:[]) <$> instanceD ctx typ [fun]
  where
    d = mkName dstr
    cons = do
        (TyConI (DataD _ _ _ cs _)) <- reify d
        return cs
    f (s, t) = match (litP $ stringL s) (normalB $ [|Just $(conE t)|]) []
    wild = match wildP (normalB [|Nothing|]) []
    typ = appT (conT ''FromText) (conT d)
    ctx = return []
