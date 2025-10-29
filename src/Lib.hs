{-# LANGUAGE OverloadedStrings #-}
module Lib
    (
    invokeFunc
    ) where

import Config (getDiscordSecret,loadChannelConfig,ChannelConfig(..))

{-
import Discord.Types
import qualified Data.Text as T
import Discord 
import UnliftIO (MonadIO(liftIO))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, when, void, unless)
import qualified Discord.Requests as R
import qualified Data.Text.IO as TIO

sendMessage :: DiscordHandler ()
sendMessage = do 
  result <- restCall $ R.CreateMessage targetChannelId (T.pack "hello world!,This is routine message.")
  case result of
    Left err -> liftIO $ putStrLn $ "Send fail,Cause:" ++ show err
    Right _ -> liftIO $ putStrLn "Send succeed."
  stopDiscord

startHandle :: DiscordHandler ()
startHandle = do 
  liftIO $ putStrLn "Bot start succeed."

  void $ liftIO $ forkIO $ forever $ do
    putStrLn "ready to send message..."
    _ <- runDiscord $ def 
        {discordToken = botToken
        , discordOnEvent = \_ -> pure ()
        , discordOnStart = sendMessage
        }

    threadDelay (sendIntervalSeconds * 1000000)

eventHandler :: Event -> DiscordHandler ()
eventHandler event = 
  case event of 
  MessageCreate m -> unless ( fromBot m ) $ do
    let content = messageContent m
    let cId = messageChannelId m

    when (content == T.pack "!ping") $ do 
      void $ restCall $ R.CreateMessage cId (T.pack "Pong!")

    when (content == T.pack "!hello") $ do 
      void $ restCall $ R.CreateMessage cId (T.pack "Hello world!")

    when (content == T.pack "!status") $ do 
      void $ restCall $ R.CreateMessage cId (T.pack "Bot is running!")

    when (content == T.pack "!stop") $ do 
      void $ restCall $ R.CreateMessage cId (T.pack "Bot is stopping,bye!")
      stopDiscord

  _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

invokeFunc :: IO ()
invokeFunc = do
  putStrLn "start discord Bot"

  userFacingError <- runDiscord $ def 
    { discordToken = botToken
    , discordOnEvent = eventHandler
    , discordOnStart = startHandle}

  TIO.putStrLn userFacingError

-}
invokeFunc::IO ()
invokeFunc = do
  res <- getDiscordSecret
  putStrLn res

  putStrLn "Get discord Secret success!"

  channel_config <- loadChannelConfig "config.toml"
  print channel_config

  putStrLn $ "Target:" ++ show (target_channel channel_config)
  putStrLn $ "Source list:" ++ show (source_channels channel_config)

