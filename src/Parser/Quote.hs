{-# LANGUAGE OverloadedStrings, TemplateHaskell  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser.Quote where

import               Data.ByteString                     (ByteString)
import               Data.String                         (IsString)
import               qualified Data.ByteString as B
import               ParserGen.Gen
import               ParserGen.Parser
import               ParserGen.Common
import               qualified ParserGen.Parser as P
import Data.List (sortBy)
import Data.Ord (comparing)


newtype IssueCode = IssueCode ByteString deriving (Eq, Show)
newtype Price = Price Int deriving (Eq, Num, Show)
type Qty = Int
type Position = (Price, Qty)
type DiffTime = (Int, Int, Int)
data MarketStatus
    = OpeningAuction
    | RegularSession
    | ClosingAuction
    | EndOfDay
    | StatusInfo String
    deriving (Eq, Show)

bidOrOfferOpt :: Parser Position
bidOrOfferOpt = (,) <$> unsafePriceX 5 <*> unsafeDecimalX 7

unsafePriceX :: Int -> Parser Price
unsafePriceX i = fromIntegral <$> unsafeDecimalX i

ts8 :: Parser DiffTime
ts8 = (,,) <$> unsafeDecimalX 2 <*> unsafeDecimalX 2 <*> unsafeDecimalX 4

marketStatus :: Parser MarketStatus
marketStatus = do
    n <- unsafeDecimalX 2
    return $ case n of
            0  -> OpeningAuction
            10 -> OpeningAuction
            11 -> RegularSession
            20 -> RegularSession
            21 -> RegularSession
            30 -> ClosingAuction
            40 -> RegularSession
            99 -> EndOfDay
            _  -> StatusInfo $ "Unknown market status: " ++ show n

$(genDataTypeFromFile "Quote.ths")
$(genParserFromFile   "Quote.ths")

-- todo more messages
chooseParserModel :: 
    (Eq a, Data.String.IsString a) =>
    a -> Parser Quote
chooseParserModel t = case t of
                            "G7034" -> parserForQuotex
                            _ -> parserForQuotey

-- probably a better way to do this
sortByTradingTime :: [Quote] -> [Quote]
sortByTradingTime = sortBy (comparing qTradingTime)


printer :: ByteString -> (Either String Quote)
printer x = parse (chooseParserModel (B.take 5 x)) x