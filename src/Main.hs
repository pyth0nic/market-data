{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where
import System.Console.CmdArgs
import Pipeline (runUdpQuotePipeline) 

data MarketDataProgram = MarketDataProgram { reorder :: Bool,
                                             hostAddress :: String, 
                                             hostPort :: Integer 
                                           }
                         deriving (Show, Data, Typeable)

marketDataProgram :: MarketDataProgram
marketDataProgram = MarketDataProgram { reorder = def &= help "Reorder records based on quote accept time",
                                        hostAddress = "127.0.0.1" &= help "The binding address",
                                        hostPort = 15515 &= help "The binding port" }

main :: IO ()
main = do
        putStrLn "Greetings, to get more info on this program and its arguments pass the -? | --help flag on run."
        args' <- cmdArgs $ modes [marketDataProgram]
                        &= help "A toy program that parses and outputs streaming udp data" 
                        &= summary "MarketData v0.0.0.1, (C) Adrian Plani" 
                        &= details ["This program uses conduit to stream and process data through a conduit pipeline",
                                    "Further extensions would be to produce to a kafka topic for further processing or",
                                    "simply add another sink that produces and outputs interesting", 
                                    "trading statistics/buy sell signals"]
        print args'
        runUdpQuotePipeline (hostAddress args') (hostPort args') (reorder args')