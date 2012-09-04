{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Test.Hspec (hspec)

import PropExample
import SpecExample

main :: IO ()
main = do
    propTest
    specTest

specTest :: IO ()
specTest = hspec normalSpecs

propTest :: IO ()
propTest = hspec propSpecs
