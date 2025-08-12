module Main (main) where

import Discord.Types
import qualified Data.Text as T
import Discord 
import UnliftIO (MonadIO(liftIO))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, when, void, unless)
import qualified Discord.Requests as R
import qualified Data.Text.IO as TIO
import Config (getDiscordSecret)

botToken :: T.Text
botToken = T.pack "DISCORD_ROBOT_TOKEN"

targetChannelId :: ChannelId
targetChannelId = read "111111111111111111"

sendIntervalSeconds :: Int
sendIntervalSeconds = 60

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

main :: IO ()
main = do
  res <- getDiscordSecret
  putStrLn res

