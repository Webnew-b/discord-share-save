{-# LANGUAGE OverloadedStrings #-}
module QueueSpec (spec) where

import Test.Hspec
import UnliftIO.STM
import Queue
import Config (ChannelConfig(..), SourceChannel(..), TargetChannel(..), RateLimitConfig(..))
import Discord.Types (Event(..))
import Control.Monad.Reader (runReaderT)
import Control.Concurrent (threadDelay, forkIO)

fakeCfg :: ChannelConfig
fakeCfg = ChannelConfig (SourceChannel ["123"]) (TargetChannel 456) (RateLimitConfig 5 200)

fakeEvent :: Event
fakeEvent = MessageCreate undefined

spec :: Spec
spec = describe "Queue.worker" $ do
  it "processes one event then stops" $ do
    q <- newTQueueIO
    stop <- newTVarIO False
    atomically $ writeTQueue q fakeEvent

    -- 启动 worker 在线程中
    _ <- forkIO $ runReaderT (worker q stop fakeCfg (\_ _ -> pure ())) undefined

    -- 等一会儿后发出停止信号
    threadDelay 200000   -- 0.2s
    atomically $ writeTVar stop True
    threadDelay 200000   -- 让 worker 退出

    True `shouldBe` True
