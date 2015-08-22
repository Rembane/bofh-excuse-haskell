module Excuse (deliverExcuse) where

import Data.List (lines)
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import System.Directory (doesFileExist)
import System.IO (readFile, writeFile)
import System.Random (getStdGen, randomR)

excuseCache = "excuses.txt"

downloadExcuses :: IO ()
downloadExcuses = (simpleHTTP $ getRequest "http://pages.cs.wisc.edu/~ballard/bofh/excuses")
    >>= getResponseBody 
    >>= (writeFile excuseCache)

deliverExcuse :: IO String
deliverExcuse = do
  hasExcuses <- doesFileExist excuseCache
  if hasExcuses
     then go
     else downloadExcuses >> go
  where
    go = do
      excuses <- fmap lines $ readFile excuseCache 
      g       <- getStdGen
      case excuses of
        [] -> error "The excuse list is empty! (This is a static excuse/error message)."
        _  -> return $ excuses !! (fst $ randomR (0, (length excuses)-1) g)

