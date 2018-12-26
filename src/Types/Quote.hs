module Types.Quote (
    Price (),
    Quantity (),
    Volume (),
    Quotes (),
    BidAsk (),
    Quote (..)
) where
import           Data.Time                      (UTCTime)
import           Data.Text                      (Text)
import           Data.Decimal                   (Decimal)

newtype Price = Price Decimal
    deriving (Show, Eq)

newtype Quantity = Quantity Decimal
    deriving (Show, Eq)

newtype Volume = Volume Integer
    deriving (Show, Eq)

newtype Quotes = Quotes Integer
    deriving (Show, Eq)

data BidAsk = BidAsk {
    price :: Price, 
    quantity :: Quantity,
    quotes :: Quotes
} deriving (Show)

data Quote = Nada | Quote {
    dataType :: Text,
    informationType :: Text,
    marketType :: Text,
    issueQuote :: Text,
    issueSeqNo :: Text,
    marketStatusType :: Text,                     
    ttlBidQuoteVolume :: Volume,
    topBids :: [BidAsk],
    ttlAskQuoteVolume :: Volume,
    topAsks :: [BidAsk],
    noBestBidValidQuote :: Integer,
    noBestAskValidQuote :: Integer,
    quoteAcceptTime :: UTCTime
    } 
    deriving (Show)