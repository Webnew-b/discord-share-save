{-# LANGUAGE OverloadedStrings #-}

module Main where

import Discord
import Discord.Types
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class (liftIO)
import System.Environment (lookupEnv)
import qualified Discord.Requests as R
import System.IO (hFlush, stdout)
import Control.Monad (void, forever)
import Control.Concurrent (threadDelay, forkIO)


getDiscordSecret :: IO T.Text
getDiscordSecret = do
  token <- fmap T.pack <$> lookupEnv "DISCORD_SECRET" 
  case token of
    Nothing -> ioError (userError "No DISCORD_SECRET found in environment.")
    Just t -> pure t


main :: IO ()
main = do
  token <- getDiscordSecret  -- 获取 token
  let testChannelId = read "1332717860622962765" :: ChannelId  -- 你的频道 ID
  
  putStrLn "========================================="
  putStrLn "Discord Bot 调试模式"
  putStrLn "========================================="
  putStrLn $ "Token 长度: " ++ show (T.length token)
  putStrLn $ "频道 ID: " ++ show testChannelId
  
  -- 使用 forkIO 确保程序不会退出
  _ <- forkIO $ do
    err <- runDiscord $ def
      { discordToken = token
      
      , discordOnStart = do
          liftIO $ do
            putStrLn "✓ WebSocket 连接成功"
            putStrLn "✓ 正在发送 IDENTIFY..."
            putStrLn "测试发送消息..."
          
          -- 测试消息
          result <- restCall $ R.CreateMessage testChannelId "🤖 Bot 连接成功！如果你看到这条消息但 Bot 显示离线，说明 Gateway 有问题。"
          case result of
            Left e -> liftIO $ putStrLn $ "❌ 发送失败: " ++ show e
            Right _ -> liftIO $ putStrLn "✓ 消息发送成功"
          
          liftIO $ do
            putStrLn "---"
            putStrLn "请检查："
            putStrLn "1. Bot 是否显示在线（绿点）"
            putStrLn "2. 在频道发送消息测试事件"
            putStrLn "---"
            hFlush stdout
      
      , discordOnEvent = \event -> do
          liftIO $ do
            putStrLn "========================================="
            putStrLn "★★★ 收到事件！★★★"
            print event
            putStrLn "========================================="
            hFlush stdout
      
      , discordOnEnd = do
          putStrLn "❌ 连接断开"
          hFlush stdout
      
      , discordOnLog = \msg -> do
          -- 打印所有日志，特别注意 READY 事件
          TIO.putStrLn $ "[GATEWAY] " <> msg
          hFlush stdout
      
      -- 启用所有必要的 Intents
      , discordGatewayIntent = def
          { gatewayIntentGuilds = True
          , gatewayIntentMembers = True  -- 尝试启用这个
          , gatewayIntentPresences = True  -- 尝试启用这个
          , gatewayIntentMessageChanges = True
          , gatewayIntentMessageContent = True
          , gatewayIntentDirectMessageChanges = True
          }
      
      , discordForkThreadForEvents = True
      , discordEnableCache = True
      }
    
    TIO.putStrLn $ "运行结束: " <> err
  
  -- 保持主线程运行
  putStrLn "Bot 运行中... 按 Ctrl+C 退出"
  forever $ do
    threadDelay 10000000  -- 10 秒
    putStrLn "."
    hFlush stdout
