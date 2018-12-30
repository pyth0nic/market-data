module Pipeline where
    
import                      Network
import                      Data.Conduit        (runConduitRes)
import                      Data.Conduit.List   (chunksOf)
import                      Data.Conduit.Combinators (iterM)
import                      Conduit
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Monoid
import Parser.Quote (printer, sortByTradingTime, Quote(..))

packStr' :: String -> B.ByteString
packStr' = encodeUtf8 . T.pack

left :: Either l r -> Maybe l
left = either Just (const Nothing)

right :: Either l r -> Maybe r
right = either (const Nothing) Just

parseResultSplitter :: Monad m => ConduitT (Either String Quote) (Either B.ByteString B.ByteString) m ()
parseResultSplitter = getZipConduit
    $ ZipConduit (concatMapC left .| mapC (\x -> packStr' $ (show x) ++ "\n") 
                                     .| mapC Left)

   *> ZipConduit (concatMapC right .| chunksOf 50
                                     .| mapC sortByTradingTime 
                                       .| mapC (\x -> packStr' $ (show x) ++ "\n" ) 
                                          .| mapC Right)

showToFile :: ConduitT (Either B.ByteString B.ByteString) Void (ResourceT IO) ()
showToFile = getZipConduit
               $  ZipConduit (concatMapC left .| sinkFile "error.txt")   
               *> ZipConduit (concatMapC right .| sinkFile "results.txt")

runUdpQuotePipeline = runConduitRes $ udpSource "127.0.0.1" 15515 600 .| mapC fst
                                                                    .| mapC printer
                                                                        .| parseResultSplitter
                                                                            .| showToFile