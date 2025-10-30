{-# LANGUAGE OverloadedStrings #-}
module Handle (handleBot) where

import Discord.Types
import qualified Data.Text as T
import Discord 
import UnliftIO (MonadIO(liftIO))
import Control.Monad ( when, void, unless)
import qualified Discord.Requests as R
import qualified Data.Text.IO as TIO
import qualified Config

eventHandler :: Config.ChannelConfig -> Event -> DiscordHandler ()
eventHandler cc event = 
  case event of 
  MessageCreate m -> unless ( fromBot m ) $ do

    liftIO $ putStrLn "Handler is running."

    let content = messageContent m
        cid = messageChannelId m
        author = userName $ messageAuthor m
        sources = Config.getSourceChannels cc
        target = Config.getTargetChannel cc
        sourcesId = map read sources

    when (cid `elem` sourcesId) $ do
      let containsURL = "http" `T.isInfixOf` content
          mentionedBot = "<@" `T.isPrefixOf` content
      when (containsURL || mentionedBot) $ do
        let msg = T.concat [author ,":",content]
        _ <- restCall (R.CreateMessage (read target) msg)
        pure ()

    when (content == T.pack "!ping") $ do 
      void $ restCall $ R.CreateMessage cid (T.pack "Pong!")

    when (content == T.pack "!status") $ do 
      void $ restCall $ R.CreateMessage cid (T.pack "Bot is running!")

  _ -> pure ()


fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

-- | handleBot argument is BotToken ChannelConfig
handleBot :: T.Text -> Config.ChannelConfig ->  IO ()
handleBot botToken cc = do
  putStrLn "Starting discord Bot"

  userFacingError <- runDiscord $ def 
    { discordToken = botToken
    , discordOnEvent = \e -> do 
        liftIO $ putStrLn("[EVENT]" ++ take 80 (show e))
        eventHandler cc e
    , discordOnStart = liftIO $ putStrLn "Robot is running"
    , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
    , discordGatewayIntent = def 
        { gatewayIntentGuilds = True
        , gatewayIntentMessageChanges = True
        , gatewayIntentDirectMessageChanges = True
        , gatewayIntentMessageContent = True
        }
    }

  TIO.putStrLn userFacingError


