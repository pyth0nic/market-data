{-# LANGUAGE OverloadedStrings #-}

module Main where

import                      Network
import                      Data.Conduit        (runConduitRes)
import                      Conduit
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Monoid
import Parser (parseQuoteMessage)
import Parser.Quote (printer)

filepath = "/Users/adrianp/haskell/tsuru/mdf-kospi200.20110216-0 (1).pcap"

packStr'' :: String -> B.ByteString
packStr'' = encodeUtf8 . T.pack

main :: IO ()
main = do 
    runConduitRes (udpSource "127.0.0.1" 15515 600 .| mapC fst 
                                                     .| mapC printer
                                                      .| printC)
