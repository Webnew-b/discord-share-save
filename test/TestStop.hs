import UnliftIO.STM
import System.Posix
import UnliftIO.Concurrent
import Control.Monad (unless, void)
import GHC.Conc.Sync (retry)

main :: IO ()
main = do
  stopFlag <- newTVarIO False
  void $ installHandler keyboardSignal (CatchOnce $ do
      putStrLn "\n⚠️  Caught Ctrl-C, stopping bot..."
      atomically $ writeTVar stopFlag True
    ) Nothing

  _ <- forkIO $ do
    let loop = do
          stop <- readTVarIO stopFlag
          putStrLn $ "[Worker] stopFlag = " ++ show stop
          unless stop $ do
            threadDelay 1000000
            loop
    loop
    putStrLn "[Worker] stopped gracefully."

  putStrLn "Press Ctrl-C to stop."

  -- 主线程等待 stopFlag 为 True 再退出
  atomically $ do
    stop <- readTVar stopFlag
    unless stop retry

  putStrLn "✅ Main thread detected stopFlag = True, exiting..."
