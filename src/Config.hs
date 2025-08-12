module Config (
  getDiscordSecret
) where

import qualified Data.Text as T
import System.Environment (lookupEnv)

-- | Get discord robot token with DISCORD_SECRET in environment
getDiscordSecret :: IO String
getDiscordSecret = do
  token <- fmap T.pack <$> lookupEnv "DISCORD_SECRET" 
  case token of
    Nothing -> ioError (userError "No DISCORD_SECRET found in environment.")
    Just t -> pure (T.unpack t)

data ChannelConfig = ChannelConfig
  {
    source_channel ::[Integer]
    ,target_channel ::Integer
    ,max_channel :: Int
  } deriving (Show)

