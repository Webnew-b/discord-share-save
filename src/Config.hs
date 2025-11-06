{-# LANGUAGE OverloadedStrings #-}

module Config (
  getDiscordSecret
  ,loadChannelConfig
  ,ChannelConfig(..)
  ,SourceChannel(..)
  ,TargetChannel(..)
  ,RateLimitConfig(..)
  ,getSourceChannels
  ,getTargetChannel
  ,getRateLimit
) where

import qualified Data.Text as T
import System.Environment (lookupEnv)
import Toml (TomlCodec,(.=),decodeFileExact)
import qualified Toml
import Discord.Types (ChannelId)
import Text.Read (readMaybe)

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
    ,rate_limit::RateLimitConfig
  } deriving (Show)

newtype SourceChannel = SourceChannel{source_channels :: [String]} deriving (Show)

newtype TargetChannel = TargetChannel {target_channel :: ChannelId}  deriving (Show) 

data RateLimitConfig = RateLimitConfig {
  max_request:: Int
  ,window_time_max :: Int
} deriving (Show)

channelIdCodec :: Toml.Key -> TomlCodec ChannelId
channelIdCodec k =
  Toml.dimatch channelIdToString channelIdFromString (Toml.string k)
  
channelIdToString :: ChannelId -> Maybe String
channelIdToString w = Just (show w)

channelIdFromString :: String -> ChannelId
channelIdFromString s =
      case readMaybe s :: Maybe ChannelId of
        Just w  -> w
        Nothing -> error $ "Invalid ChannelId: " ++ s

sourceChannelCodec :: TomlCodec SourceChannel
sourceChannelCodec = SourceChannel
  <$> Toml.arrayOf Toml._String "sources" .= source_channels

targetChannelCodec :: TomlCodec TargetChannel
targetChannelCodec = TargetChannel
  <$> channelIdCodec "channel" .= target_channel

rateLimitCodec :: TomlCodec RateLimitConfig
rateLimitCodec = RateLimitConfig
  <$> Toml.int "max_request" .= max_request
  <*> Toml.int "window_time_max" .= window_time_max

channelConfigCodec :: TomlCodec ChannelConfig
channelConfigCodec = ChannelConfig
  <$> Toml.table sourceChannelCodec  "source" .= source
  <*> Toml.table targetChannelCodec "target" .= target
  <*> Toml.table rateLimitCodec "rate_limit" .= rate_limit

getSourceChannels :: ChannelConfig -> [String]
getSourceChannels = source_channels . source

getTargetChannel :: ChannelConfig -> ChannelId
getTargetChannel = target_channel . target

getRateLimit :: ChannelConfig -> (Int,Int)
getRateLimit =(,) <$> (max_request . rate_limit) <*> (window_time_max . rate_limit)

loadChannelConfig :: FilePath -> IO ChannelConfig
loadChannelConfig path = do
  result <- decodeFileExact channelConfigCodec path
  case result of
    Left err -> ioError (userError $ "Toml decode error:\n" ++ show err)
    Right cfg -> do
      let targetChannel = getTargetChannel cfg
          sourceChannels = map channelIdFromString (getSourceChannels cfg)
      if targetChannel `elem` sourceChannels then
        ioError (userError "Source channel should not include target channel.")
      else
        pure cfg
