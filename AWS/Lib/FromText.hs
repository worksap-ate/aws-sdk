{-# LANGUAGE RankNTypes, OverloadedStrings, TemplateHaskell, FlexibleInstances, CPP #-}

module AWS.Lib.FromText
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
#if MIN_VERSION_conduit(1,1,0)
import Control.Monad.Trans.Resource (MonadThrow)
#else
import Data.Conduit (MonadThrow)
#endif
import Data.IP (IPv4, AddrRange)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.Time as Time
import qualified Data.Time.Parse as TP
import Language.Haskell.TH
import Safe

import AWS.Class

class Read a => FromText a
  where
    fromText :: MonadThrow m => Text -> m a
    fromText t
        = maybe (monadThrow $ FromTextError t) return
        . fromTextMay
        $ t

    fromTextMay :: Text -> Maybe a
    fromTextMay = readMay . T.unpack

    fromMaybeText :: MonadThrow m => Text -> Maybe Text -> m a
    fromMaybeText name
        = maybe
            (monadThrow $ FromTextError $ "no text: " <> name)
            fromText

instance FromText a => FromText (Maybe a)
  where
    fromText = return . join . fromTextMay
    fromMaybeText _name Nothing  = return Nothing
    fromMaybeText _name (Just t) = return $ fromTextMay t
    fromTextMay = Just . fromTextMay

instance FromText Int
instance FromText Integer
instance FromText Double
instance FromText IPv4
instance FromText (AddrRange IPv4)

instance FromText Text
  where
    fromTextMay t
        | t == ""   = Nothing
        | otherwise = Just t

instance FromText Bool
  where
    fromTextMay "true"  = Just True
    fromTextMay "false" = Just False
    fromTextMay _       = Nothing

instance FromText UTCTime
  where
    fromTextMay t
        = Time.localTimeToUTC Time.utc . fst
        <$> (TP.strptime fmt $ T.unpack t)
      where
        fmt = "%FT%T"

deriveFromText :: String -> [String] -> DecsQ
deriveFromText dstr strs = do
    ctrs <- map (\(NormalC name _) -> name) <$> cons
    x <- newName "x"
    let cases = caseE (varE x) (map f (zip strs ctrs) ++ [wild])
    let fun = funD 'fromTextMay [clause [varP x] (normalB cases) []]
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
