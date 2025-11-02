{-# LANGUAGE ScopedTypeVariables #-}

module Queue (
  worker
  ,enqueueEvent
  ,outqueneEvent
  ,startWorker
) where

import UnliftIO
import Discord
import Discord.Types
import Config (ChannelConfig)
import Control.Concurrent
import Control.Monad (unless)
import Data.Functor (void)
import Control.Monad.Reader (ask, runReaderT)

type EventProcess = (ChannelConfig -> Event -> DiscordHandler ())

enqueueEvent :: TQueue Event -> Event -> DiscordHandler ()
enqueueEvent q ev = liftIO . atomically $ writeTQueue q ev

outqueneEvent :: TQueue Event -> DiscordHandler Event
outqueneEvent q = liftIO . atomically $ readTQueue q 



worker :: TQueue Event -> TVar Bool -> ChannelConfig -> EventProcess -> DiscordHandler ()
worker q stopFlag cfg f = do
  liftIO $ putStrLn "[Worker] started."
  let loop = do
        stop <- liftIO . atomically $ readTVar stopFlag
        unless stop $ do
          ev <- liftIO . atomically $ readTQueue q
          f cfg ev `catch` \(e :: SomeException) ->
            liftIO $ putStrLn $ "[Worker] Event handler crashed: " ++ show e
          liftIO $ threadDelay 500000
          loop
  loop
  liftIO $ putStrLn "[Worker] stopped gracefully."

startWorker :: TQueue Event -> TVar Bool -> ChannelConfig -> EventProcess -> DiscordHandler ()
startWorker q stopFlag cfg f = do
  handleM <- ask
  liftIO $ putStrLn "[Main] Spawning worker thread..."
  void . liftIO . async $ runReaderT (worker q stopFlag cfg f) handleM
  pure ()
