{-# LANGUAGE DeriveDataTypeable #-}
module Config where

import System.Console.CmdArgs

data MarketDataConfig = MarketDataConfig { 
                                          reorder :: Bool,
                                          fileOutput :: Bool,
                                          hostAddress :: String, 
                                          hostPort :: Integer
                                        }
                        deriving (Show, Data, Typeable)