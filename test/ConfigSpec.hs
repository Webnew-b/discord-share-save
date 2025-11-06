{-# LANGUAGE OverloadedStrings #-}
module ConfigSpec (spec) where

import Test.Hspec
import Config
import System.IO.Temp (withSystemTempFile)
import qualified Data.Text.IO as TIO
import qualified Data.Text as Text
import System.IO (hClose)

spec :: Spec
spec = describe "Config.loadChannelConfig" $ do

  it "parses valid toml config successfully" $ do
    let tomlContent = unlines
          [ "[source]"
          , "sources = [\"111111111111111111\", \"222222222222222222\"]"
          , "[target]"
          , "channel = \"333333333333333333\""
          , "[rate_limit]"
          , "max_request = 10"
          , "window_time_max = 5000"
          ]
    withSystemTempFile "config.toml" $ \path h -> do
      TIO.hPutStr h (Text.pack tomlContent)
      hClose h
      cfg <- loadChannelConfig path
      let (maxReq, windowMax) = getRateLimit cfg
      getSourceChannels cfg `shouldBe` ["111111111111111111", "222222222222222222"]
      show (getTargetChannel cfg) `shouldContain` "333333333333333333"
      maxReq `shouldBe` 10
      windowMax `shouldBe` 5000

  it "throws error when target channel is also in source list" $ do
    let tomlContent = unlines
          [ "[source]"
          , "sources = [\"123456789012345678\"]"
          , "[target]"
          , "channel = \"123456789012345678\""
          , "[rate_limit]"
          , "max_request = 5"
          , "window_time_max = 1000"
          ]
    withSystemTempFile "config.toml" $ \path h -> do
      TIO.hPutStr h (Text.pack tomlContent)
      hClose h
      loadChannelConfig path `shouldThrow` anyIOException
