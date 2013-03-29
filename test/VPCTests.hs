module Main where

import EC2Tests.VPCTests

main :: IO ()
main = do
    runVpcTests
