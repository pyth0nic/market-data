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

filepath = "/Users/adrianp/haskell/tsuru/mdf-kospi200.20110216-0 (1).pcap"

packStr'' :: String -> B.ByteString
packStr'' = encodeUtf8 . T.pack

main :: IO ()
main = do 
    runConduitRes (udpSource "127.0.0.1" 15515 500 .| mapC fst 
                                                    .| mapC (\x -> (x <> (packStr'' "wtf"))) 
                                                     .| mapC (\x -> (packStr'' (show $ parseQuoteMessage x)))
                                                      .| printC)
