{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import AWS.EC2Tests

main :: IO ()
main = do
    runInstanceTests
    runVpcTests
