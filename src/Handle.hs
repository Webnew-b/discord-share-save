{-# LANGUAGE OverloadedStrings #-}
module Handle (handleBot,eventHandler) where

import Discord.Types
import qualified Data.Text as T
import Discord
import UnliftIO (MonadIO(liftIO))
import Control.Monad ( when, void, unless)
import qualified Discord.Requests as R
import qualified Data.Text.IO as TIO
import qualified Config
import UnliftIO.STM
    ( newTQueueIO,
      TQueue,
      putTMVar,
      atomically,
      TMVar, newEmptyTMVarIO )
import Queue (enqueueEvent)
import Control.Concurrent.Async (Async,async)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Reader (ask)

eventHandler :: Config.ChannelConfig -> Event -> DiscordHandler ()
eventHandler cc event =
  case event of
  MessageCreate m -> unless ( fromBot m ) $ do

    liftIO $ putStrLn "Event is proccessing..."

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
        _ <- restCall (R.CreateMessage target msg)
        pure ()

    when (content == T.pack "!ping") $ do
      void $ restCall $ R.CreateMessage cid (T.pack "Pong!")

    when (content == T.pack "!status") $ do
      void $ restCall $ R.CreateMessage cid (T.pack "Bot is running!")

  _ -> pure ()


fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

type HandledResType = IO(Async T.Text,TMVar (),TQueue Event,DiscordHandle)

-- | handleBot argument is BotToken ChannelConfig
handleBot :: T.Text -> HandledResType
handleBot botToken = do
  putStrLn "Starting discord Bot"
  connected <- newEmptyTMVarIO
  handleVar <- newEmptyMVar
  q <- newTQueueIO

  userFacingError <- async $ runDiscord $ def
    { discordToken = botToken
    , discordOnEvent = \e -> do
        liftIO $ putStrLn ("[EVENT]" ++ take 80 (show e))
        case e of
          MessageCreate _ -> enqueueEvent q e
          _ -> pure ()
    , discordOnStart =  do
        liftIO $ putStrLn "[Discord] connected"
        atomically $ putTMVar connected ()
        handle <- ask
        liftIO $ putMVar handleVar handle 
    , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
    , discordGatewayIntent = def
        { gatewayIntentGuilds = True
        , gatewayIntentMessageChanges = True
        , gatewayIntentDirectMessageChanges = True
        , gatewayIntentMessageContent = True
        }
    }

  handleM <- takeMVar handleVar

  pure (userFacingError,connected,q,handleM)

