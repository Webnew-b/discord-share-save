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
import Control.Concurrent.Async (waitAnyCatchCancel, race)
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
  stopF <- newTVarIO False

  void $ installHandler keyboardSignal (CatchOnce $ do
      putStrLn "\n Caught Ctrl-C, stopping bot..."
      atomically $ writeTVar stopF True
      atomically $ putTMVar stopFlag ()
    ) Nothing

  (botAsync,connected,q,handle) <- handleBot botToken 

  atomically $ takeTMVar connected


  putStrLn "[Main] Discord connected,starting worker"
  runReaderT ( startWorker q stopF channel_config eventHandler) handle

  res <- race
    (atomically $ takeTMVar stopFlag)
    (waitAnyCatchCancel [botAsync])

  case res of
    Left _ -> putStrLn "Main thread received Ctrl-C, shutting down..."
    Right _ -> do
      putStrLn "Bot async exited, signaling worker to stop..."
      atomically $ do
        writeTVar stopF True
        void $ tryPutTMVar stopFlag ()

  putStrLn "Main thread detected stopFlag = True, exiting..."

