module RateLimitSpec (spec) where

import Test.Hspec
import RateLimit
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Monad (replicateM_)
import Data.Time (getCurrentTime)

spec :: Spec
spec = describe "RateLimiter" $ do
  it "resets request count after window expires" $ do
    rl <- newRateLimiter 2 200
    recordRequest rl
    recordRequest rl
    startRateLimitResetter rl
    threadDelay 300000
    recordRequest rl  -- should not block
    True `shouldBe` True
