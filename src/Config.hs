{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Config where

import            System.Console.CmdArgs
import            Data.IP                     (IP)
import Text.Read
import Data.Maybe (isJust)
import Control.Monad (guard)

data MarketDataConfig = MarketDataConfig { 
                                          reorder :: Bool,
                                          fileOutput :: Bool,
                                          hostAddress :: String, 
                                          hostPort :: Integer
                                         }
                        deriving (Show, Data, Typeable)

readIpAddress :: String -> Maybe IP
readIpAddress ip = readMaybe ip :: Maybe IP

validateHostPort :: Integer -> Bool
validateHostPort port | port <= 65535 && port > 0 = True
                      | otherwise                 = False

validConfig :: MarketDataConfig -> Bool
validConfig config = isJust $ do
  _ <- readMaybe @IP (hostAddress config)
  guard (hostPort config <= 65535 && hostPort config > 0)  

