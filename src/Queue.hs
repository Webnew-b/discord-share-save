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
import Control.Monad (unless)
import Data.Functor (void)
import Control.Monad.Reader (ask, runReaderT)
import RateLimit (newRateLimiter, waitIfLimited, recordRequest, startRateLimitResetter)

type EventProcess = (ChannelConfig -> Event -> DiscordHandler ())

enqueueEvent :: TQueue Event -> Event -> DiscordHandler ()
enqueueEvent q ev = liftIO . atomically $ writeTQueue q ev

outqueneEvent :: TQueue Event -> DiscordHandler Event
outqueneEvent q = liftIO . atomically $ readTQueue q 



worker :: TQueue Event -> TVar Bool -> ChannelConfig -> EventProcess -> DiscordHandler ()
worker q stopFlag cfg f = do
  liftIO $ putStrLn "[Worker] started."
  limiter <- liftIO $ newRateLimiter 10 5000
  liftIO $ startRateLimitResetter limiter
  let loop = do
        stop <- liftIO . atomically $ readTVar stopFlag
        unless stop $ do
          ev <- liftIO . atomically $ readTQueue q
          liftIO $ waitIfLimited limiter
          f cfg ev `catch` \(e :: SomeException) ->
            liftIO $ putStrLn $ "[Worker] Event handler crashed: " ++ show e
          liftIO $ recordRequest limiter
          loop
  loop
  liftIO $ putStrLn "[Worker] stopped gracefully."

startWorker :: TQueue Event -> TVar Bool -> ChannelConfig -> EventProcess -> DiscordHandler ()
startWorker q stopFlag cfg f = do
  handleM <- ask
  liftIO $ putStrLn "[Main] Spawning worker thread..."
  void . liftIO . async $ runReaderT (worker q stopFlag cfg f) handleM
  pure ()
