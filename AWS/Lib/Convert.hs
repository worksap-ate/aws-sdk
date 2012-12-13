{-# LANGUAGE RankNTypes, OverloadedStrings, TemplateHaskell #-}

module AWS.Lib.Convert
    where

import Language.Haskell.TH
import Control.Applicative ((<$>))

import AWS.Class

deriveFromText :: Name -> [String] -> DecsQ
deriveFromText d strs = do
    ctrs <- map (\(NormalC name _) -> name) <$> cons
    x <- newName "x"
    let cases = caseE (varE x) (map f (zip strs ctrs) ++ [wild])
    let fun = funD 'fromTextMay [clause [varP x] (normalB cases) []]
    (:[]) <$> instanceD ctx typ [fun]
  where
    cons = do
        (TyConI (DataD _ _ _ cs _)) <- reify d
        return cs
    f (s, t) = match (litP $ stringL s) (normalB $ [|Just $(conE t)|]) []
    wild = match wildP (normalB [|Nothing|]) []
    typ = appT (conT ''FromText) (conT d)
    ctx = return []
