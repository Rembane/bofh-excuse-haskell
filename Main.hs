module Main where

import Data.List (lines)
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import System.Directory (doesFileExist)
import System.IO (readFile, writeFile)
import System.Random (getStdGen, randomR)

excuseCache = "excuses.txt"

downloadExcuses :: IO ()
downloadExcuses = do 
  body <- (simpleHTTP $ getRequest "http://pages.cs.wisc.edu/~ballard/bofh/excuses") >>= getResponseBody 
  writeFile excuseCache body

deliverExcuse :: IO ()
deliverExcuse = do
  excuses <- fmap lines $ readFile excuseCache
  g       <- getStdGen
  case excuses of
    [] -> error "The excuse list is empty! (This is a static excuse/error message)."
    _  -> putStrLn $ excuses !! (fst $ randomR (0, (length excuses)-1) g)

main = do
  hasExcuses <- doesFileExist excuseCache
  if hasExcuses
     then deliverExcuse
     else downloadExcuses >> deliverExcuse 

