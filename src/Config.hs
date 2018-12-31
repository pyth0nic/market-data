{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config where

import            System.Console.CmdArgs
import            Data.IP                     (IP)
import Control.Monad                          (liftM)
import Control.Exception

data MarketDataConfig = MarketDataConfig { 
                                          reorder :: Bool,
                                          fileOutput :: Bool,
                                          hostAddress :: String, 
                                          hostPort :: Integer
                                         }
                        deriving (Show, Data, Typeable)

tryParseIpAddress :: Read IP => String -> IO IP
tryParseIpAddress ip = readIO ip

validateHostPort :: Integer -> Bool
validateHostPort port | port <= 65535 && port > 0 = True
                      | otherwise                 = False

-- TODO This is ugly use an error type to keep track of program errors
validConfig :: MarketDataConfig -> IO (Bool)
validConfig config = do
                        let port = validateHostPort $ hostPort config
                        case port of
                          False -> pure False
                          True  -> do
                                    addr <- catch (Right `liftM` (tryParseIpAddress $ hostAddress config))
                                                  (\(e :: IOError) -> do putStrLn $ show e
                                                                         pure $ Left e)
                                    case addr of
                                      Left _ -> do 
                                                  putStrLn "IP Address parse error"
                                                  pure False
                                      Right _ -> pure True