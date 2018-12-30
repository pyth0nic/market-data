{-# LANGUAGE OverloadedStrings #-}

module Main where
import Pipeline (runUdpQuotePipeline)

main :: IO ()
main = do
        runUdpQuotePipeline
    