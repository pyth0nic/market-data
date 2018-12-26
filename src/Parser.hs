module Parser (
    parseQuoteMessage
    ) where

import Types.Quote (Quote (..))
import Data.ByteString

parseQuoteMessage :: ByteString -> Quote
parseQuoteMessage msg = Nada