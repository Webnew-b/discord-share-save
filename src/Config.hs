{-# LANGUAGE OverloadedStrings #-}

module Config (
  getDiscordSecret
  ,loadChannelConfig
  ,ChannelConfig(..)
  ,SourceChannel(..)
  ,TargetChannel(..)
  ,getSourceChannels
  ,getTargetChannel
) where

import qualified Data.Text as T
import System.Environment (lookupEnv)
import Toml (TomlCodec,(.=),decodeFileExact)
import qualified Toml

-- | Get discord robot token with DISCORD_SECRET in environment
getDiscordSecret :: IO T.Text
getDiscordSecret = do
  token <- fmap T.pack <$> lookupEnv "DISCORD_SECRET" 
  case token of
    Nothing -> ioError (userError "No DISCORD_SECRET found in environment.")
    Just t -> pure t

data ChannelConfig = ChannelConfig
  {
    source ::SourceChannel
    ,target :: TargetChannel
  } deriving (Show)

newtype SourceChannel = SourceChannel{source_channels :: [String]} deriving (Show)

newtype TargetChannel = TargetChannel {target_channel :: String}  deriving (Show) 

sourceChannelCodec :: TomlCodec SourceChannel
sourceChannelCodec = SourceChannel
  <$> Toml.arrayOf Toml._String "sources" .= source_channels

targetChannelCodec :: TomlCodec TargetChannel
targetChannelCodec = TargetChannel
  <$> Toml.string "channel" .= target_channel

channelConfigCodec :: TomlCodec ChannelConfig
channelConfigCodec = ChannelConfig
  <$> Toml.table sourceChannelCodec  "source" .= source
  <*> Toml.table targetChannelCodec "target" .= target

getSourceChannels :: ChannelConfig -> [String]
getSourceChannels = source_channels . source

getTargetChannel :: ChannelConfig -> String
getTargetChannel = target_channel . target


loadChannelConfig :: FilePath -> IO ChannelConfig
loadChannelConfig path = do
  result <- decodeFileExact channelConfigCodec path
  case result of
    Left err -> ioError (userError $ "Toml decode error:\n" ++ show err)
    Right cfg -> do
      let targetChannel = getTargetChannel cfg
          sourceChannels = getSourceChannels cfg
      if targetChannel `elem` sourceChannels then
        ioError (userError "Source channel should not include target channel.")
      else
        pure cfg
