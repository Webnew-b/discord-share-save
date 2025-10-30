{-# LANGUAGE OverloadedStrings #-}
module Lib
    (
    invokeFunc
    ) where

import qualified Config
import Handle (handleBot)

invokeFunc::IO ()
invokeFunc = do
  botToken <- Config.getDiscordSecret

  putStrLn "Get discord Secret success!"

  channel_config <- Config.loadChannelConfig "config.toml"
  print channel_config

  putStrLn $ "Target:" ++ show (Config.getTargetChannel channel_config)
  putStrLn $ "Source list:" ++ show (Config.getSourceChannels channel_config)

  handleBot botToken channel_config 

