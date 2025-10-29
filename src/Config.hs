{-# LANGUAGE OverloadedStrings #-}

module Config (
  getDiscordSecret
  ,loadChannelConfig
  ,ChannelConfig(..)
) where

import qualified Data.Text as T
import System.Environment (lookupEnv)
import Toml (TomlCodec,(.=),decodeFileExact)
import qualified Toml

-- | Get discord robot token with DISCORD_SECRET in environment
getDiscordSecret :: IO String
getDiscordSecret = do
  token <- fmap T.pack <$> lookupEnv "DISCORD_SECRET" 
  case token of
    Nothing -> ioError (userError "No DISCORD_SECRET found in environment.")
    Just t -> pure (T.unpack t)

data ChannelConfig = ChannelConfig
  {
    source_channels ::[Integer]
    ,target_channel ::Integer
    ,max_channel :: Int
  } deriving (Show)

channelConfigCodex :: TomlCodec ChannelConfig
channelConfigCodex = ChannelConfig
  <$> Toml.arrayOf Toml._Integer "bot.source_channels" .= source_channels
  <*> Toml.integer "bot.target_channel" .= target_channel
  <*> Toml.int "bot.max_channels" .= max_channel
  
loadChannelConfig :: FilePath -> IO ChannelConfig
loadChannelConfig path = do
  result <- decodeFileExact channelConfigCodex path
  case result of
    Left err -> ioError (userError $ "Toml decode error:\n" ++ show err)
    Right cfg -> pure cfg
