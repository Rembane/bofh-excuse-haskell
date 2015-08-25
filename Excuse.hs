module Excuse (deliverExcuse, excuseList) where

import Data.List (lines)
import qualified Data.Vector as V
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import System.Directory (doesFileExist)
import System.IO (readFile, writeFile)
import System.Random (getStdGen, randomR, randomRs)

excuseCache = "excuses.txt"

-- | Download the excuses from the excuse-list.
downloadExcuses :: IO ()
downloadExcuses = (simpleHTTP $ getRequest "http://pages.cs.wisc.edu/~ballard/bofh/excuses")
    >>= getResponseBody 
    >>= (writeFile excuseCache)

-- | Returns an infinite list of excuses in random order.
excuseList :: IO [String]
excuseList = do
  hasExcuses <- doesFileExist excuseCache
  if hasExcuses
     then go
     else downloadExcuses >> go
  where
    go = do
      excuses <- fmap (V.fromList . lines) $ readFile excuseCache
      g       <- getStdGen
      if V.null excuses
         then error "The excuse list is empty! (This is a static excuse/error message)."
         else return $ infList g excuses
    infIndices g excuses = randomRs (0, (length excuses)-1) g
    infList g excuses = map (excuses V.!) (infIndices g excuses)

-- | Deliver one excuse.
deliverExcuse :: IO String
deliverExcuse = fmap head excuseList
