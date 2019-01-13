{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Config where

import           Control.Monad          (guard)
import           Data.IP                (IP)
import           Data.Maybe             (isJust)
import           System.Console.CmdArgs
import           Text.Read

data MarketDataConfig = MarketDataConfig {
                                          reorder       :: Bool,
                                          fileOutput    :: Bool,
                                          fileInputPath :: FilePath,
                                          hostAddress   :: String,
                                          hostPort      :: Integer
                                         }
                        deriving (Show, Data, Typeable)

validateHostPort :: Integer -> Bool
validateHostPort port | port <= 65535 && port > 0 = True
                      | otherwise                 = False

validConfig :: MarketDataConfig -> Bool
validConfig config = isJust $ do
  _ <- readMaybe @IP (hostAddress config)
  guard (hostPort config <= 65535 && hostPort config > 0)

