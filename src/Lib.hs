{-# LANGUAGE OverloadedStrings #-}
module Lib
    (
    invokeFunc
    ) where

import qualified Config
import Handle (handleBot, eventHandler)
import UnliftIO.STM
import System.Posix
import Control.Monad (void)
import UnliftIO.Async (race_)
import Control.Concurrent.Async (waitAnyCatchCancel)
import Queue (startWorker)
import Control.Monad.Reader (runReaderT)

invokeFunc::IO ()
invokeFunc = do
  botToken <- Config.getDiscordSecret

  putStrLn "Get discord Secret success!"

  channel_config <- Config.loadChannelConfig "config.toml"
  print channel_config

  putStrLn $ "Target:" ++ show (Config.getTargetChannel channel_config)
  putStrLn $ "Source list:" ++ show (Config.getSourceChannels channel_config)

  stopFlag <- newEmptyTMVarIO
  void $ installHandler keyboardSignal (CatchOnce $ do
      putStrLn "\n Caught Ctrl-C, stopping bot..."
      atomically $ putTMVar stopFlag ()
    ) Nothing

  (botAsync,connected,q,handle) <- handleBot botToken 

  atomically $ takeTMVar connected


  putStrLn "[Main] Discord connected,starting worker"

  stopF <- newTVarIO False
  runReaderT ( startWorker q stopF channel_config eventHandler) handle

  race_
    (atomically $ takeTMVar stopFlag)
    (waitAnyCatchCancel [botAsync])
  putStrLn "Main thread detected stopFlag = True, exiting..."

