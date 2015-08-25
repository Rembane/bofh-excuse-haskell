module Main where

import Excuse

main :: IO ()
main = deliverExcuse >>= putStrLn 

