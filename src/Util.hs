module Util (eitherToIO) where

eitherToIO :: Show e => Either e a -> IO a
eitherToIO = either (ioError.userError.show) pure

