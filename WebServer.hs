{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (pack)

import Control.Concurrent.Chan.Unagi.Bounded
import Network.Wai.Middleware.RequestLogger
import Web.Scotty

import Excuse

excuseThread :: InChan String -> IO ()
excuseThread c = do
  excuseList >>= (writeList2Chan c)

main :: IO ()
main = do
  (inChan, outChan) <- newChan 10
  forkIO (excuseThread inChan)
  scotty 8000 $ do
    middleware logStdoutDev
    get "/" $ do
      (liftIO $ readChan outChan) >>= (\e -> html $ pack e)

