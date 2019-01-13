{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Parser.Quote where

import           Data.ByteString  (ByteString)
import qualified Data.ByteString  as B
import           Data.List        (sortBy)
import           Data.Ord         (comparing)
import           Data.String      (IsString)
import           ParserGen.Common
import           ParserGen.Gen
import           ParserGen.Parser


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
    a -> Either a (Parser Quote)
chooseParserModel t = case t of
                            "G7034" -> Right parserForLongQuote
                            "B6034" -> Right parserForShortQuote
                            _ -> Left t

-- ensure each quote constructor is followed by 'q' for code generation
sortByTradingTime :: [Quote] -> [Quote]
sortByTradingTime = sortBy (comparing qTradingTime)

parseQuote :: ByteString -> (Either String Quote)
parseQuote x = case (chooseParserModel (B.take 5 x)) of
                Left s -> Left $ "No parser for packet found for " ++ (show s)
                Right p -> parse p x
