module Main where

import ELBTests.LoadBalancerTests

main :: IO ()
main = do
    runLoadBalancerTests
