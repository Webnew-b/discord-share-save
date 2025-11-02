module RateLimit (
  RateLimiter(..)
  ,newRateLimiter
  ,waitIfLimited
  ,recordRequest
  ,resetWindow
  ,startRateLimitResetter
) where
import Data.Time (getCurrentTime, diffUTCTime)
import UnliftIO
import Data.Time.Clock (UTCTime)
import GHC.Conc.Sync (retry)
import Control.Monad (when, forever)
import Data.Functor (void)
import Control.Concurrent (threadDelay)

data RateLimiter = RateLimiter {
  maxRequests :: Int
  ,timeWindow :: Int
  ,requestCount:: TVar Int
  ,windowStart :: TVar UTCTime
}

newRateLimiter :: Int -> Int -> IO RateLimiter
newRateLimiter maxReqs windowMx = do
  now <- getCurrentTime
  RateLimiter maxReqs windowMx
    <$> newTVarIO 0
    <*> newTVarIO now

waitIfLimited :: RateLimiter -> IO ()
waitIfLimited rl = 
  atomically $ do
     count <- readTVar (requestCount rl)
     when (count >= maxRequests rl) retry


recordRequest :: RateLimiter -> IO ()
recordRequest rl = atomically $ modifyTVar' (requestCount rl) (+1)

resetWindow :: RateLimiter -> IO ()
resetWindow rl = do
  now <- getCurrentTime
  atomically $ do
    writeTVar (requestCount rl) 0
    writeTVar (windowStart rl) now
    modifyTVar' (requestCount rl) id
    
startRateLimitResetter :: RateLimiter -> IO ()
startRateLimitResetter rl = void . async $ forever $  do
  now <- getCurrentTime
  start <- atomically $ readTVar (windowStart rl)
  let elapsedMs = realToFrac (diffUTCTime now start) * 1000 :: Double
  when (elapsedMs > fromIntegral (timeWindow rl)) $ do
    putStrLn "[worker] reset windows"
    resetWindow rl
  threadDelay 100000

