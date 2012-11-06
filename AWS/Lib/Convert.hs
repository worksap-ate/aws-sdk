{-# LANGUAGE RankNTypes, OverloadedStrings, TemplateHaskell #-}

module AWS.Lib.Convert
    where

import Language.Haskell.TH
import Control.Applicative ((<$>))
import Data.Text (Text)

import AWS.Util

mkConvertFunc :: String -> Name -> [String] -> Q [Dec]
mkConvertFunc fs d strs = runQ $ do
  v <- newName "v"
  ctrs <- (\(TyConI (DataD [] _ [] x [])) -> map (\(NormalC name []) -> name) x)
          <$> reify d
  return $
      [ SigD (mkName fs) (AppT (AppT ArrowT (ConT ''Text)) (ConT d))
      , FunD (mkName fs) [Clause [VarP v] (NormalB
        (CaseE (VarE v)
           $ (map (\(s,t) -> Match (LitP (StringL s)) (NormalB (ConE t)) []) $ zip strs ctrs)
           ++ [Match WildP (NormalB (AppE (AppE (VarE 'err) (LitE (StringL $ show d))) (LitE (StringL $ show v)))) []])
      ) []]
      ]
