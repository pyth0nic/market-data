{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where
import System.Console.CmdArgs
import Pipeline (runUdpQuotePipeline)
import Config

marketDataConfig :: MarketDataConfig
marketDataConfig = MarketDataConfig { reorder = def &= help "Reorder records based on quote accept time",
                                      fileOutput = False &= help "Output to files, if False (default), use commandline.",
                                      hostAddress = "127.0.0.1" &= help "The binding address",
                                      hostPort = 15515 &= help "The binding port" }

getConfig :: IO MarketDataConfig
getConfig = cmdArgs $ modes [marketDataConfig]
                &= help "A toy program that parses and outputs streaming udp data" 
                &= summary "MarketData v0.0.0.1, (C) Adrian Plani" 
                &= details ["This program uses conduit to stream and process data through a conduit pipeline",
                        "Further extensions would be to produce to a kafka topic for further processing or",
                        "simply add another sink that produces and outputs interesting", 
                        "trading statistics/buy sell signals"]

main :: IO ()
main = do
        putStrLn "Greetings, to get more info on this program and its arguments pass the -? | --help flag on run."
        config <- getConfig
        let valid = validConfig config
        case valid of
                False -> putStrLn "Invalid arguments passed, please try again."
                True -> do
                           print config
                           -- most pipeline errors are handle with an either 
                           -- TODO add error handling for network issues
                           runUdpQuotePipeline config
