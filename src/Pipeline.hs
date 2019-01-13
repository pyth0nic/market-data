module Pipeline where

import           Conduit
import           Config
import qualified Data.ByteString     as B
import           Data.Conduit        (runConduitRes)
import qualified Data.Conduit.Binary as CB
import           Data.Conduit.List   (chunksOf)
import qualified Data.Text           as T
import           Data.Text.Encoding  (encodeUtf8)
import           FileCombinator
import           Network
import           Parser.Quote        (Quote (..), parseQuote, sortByTradingTime)

packStr' :: String -> B.ByteString
packStr' = encodeUtf8 . T.pack

-- Everything passes through but sink filters out Nothing values
left :: Either l r -> Maybe l
left = either Just (const Nothing)

right :: Either l r -> Maybe r
right = either (const Nothing) Just

parseResultSplitter :: Monad m => Bool -> ConduitT (Either String Quote) (Either B.ByteString B.ByteString) m ()
parseResultSplitter reorder' = getZipConduit
    $ ZipConduit (concatMapC left .| mapC (\x -> packStr' $ show x ++ "\n") -- todo add raw data to error message
                                     .| mapC Left)

   *> ZipConduit (concatMapC right .| maybeSortAndPrint reorder' .| mapC Right)

maybeSortAndPrint :: Monad m => Bool -> ConduitT Quote B.ByteString m ()
maybeSortAndPrint True  = sortAndPrint
maybeSortAndPrint False = mapC (\x -> packStr' $ show x ++ "\n")

-- In this data set there is the potential of up to around 20 out of order entries based on window size of 900
-- TODO to solve this dechunk and then chunk to pairs and sort
-- TODO chunksOf as a timewindow would be nice
sortAndPrint :: Monad m => ConduitT Quote B.ByteString m ()
sortAndPrint = chunksOf 900 -- about 300 events/second, (3 second ordering window)
                .| mapC sortByTradingTime
                    .| mapC (\x -> packStr' (concat $ map (\x' -> (show x') ++ "\n") x))

showToFile :: ConduitT (Either B.ByteString B.ByteString) Void (ResourceT IO) ()
showToFile = getZipConduit
               $  ZipConduit (concatMapC left .| sinkFile "error.txt")
               *> ZipConduit (concatMapC right .| sinkFile "results.txt")

toFilePipeline :: Bool -> ConduitM B.ByteString Void (ResourceT IO) ()
toFilePipeline reorder' = mapC parseQuote
                                .| parseResultSplitter reorder'
                                    .| showToFile

toCmdPipeline :: MonadIO m => Bool -> ConduitM B.ByteString c m ()
toCmdPipeline reorder' = mapC parseQuote
                                .| parseResultSplitter reorder'
                                   .| printC

runUdpQuotePipeline :: MarketDataConfig -> IO ()
runUdpQuotePipeline config = do
                                case (fileOutput config) of
                                    True  -> runConduitRes $ udpSource addr port 500 .| mapC fst .| toFilePipeline reorder'
                                    False -> runConduitRes $ udpSource addr port 500 .| mapC fst .| toCmdPipeline reorder'
                                  where
                                    addr = hostAddress config
                                    port = fromInteger $ hostPort config
                                    reorder' = reorder config

parsePacketStream :: MonadResource m => FilePath -> ConduitM a B.ByteString m ()
parsePacketStream filePath = CB.sourceFile filePath  .| accumUntilChar 255
                                                     .| mapC (\x -> x <> B.singleton 255)
                                                     .| mapC (\x-> B.drop 77 x)

runFilePipeline :: MarketDataConfig -> IO ()
runFilePipeline config = do
                            case (fileOutput config) of
                                True  -> runConduitRes $ parsePacketStream filePath
                                                        .| toFilePipeline reorder'
                                False -> runConduitRes $ parsePacketStream filePath
                                                        .| toCmdPipeline reorder'
                                where
                                reorder' = reorder config
                                filePath = fileInputPath config

runPipeline :: MarketDataConfig -> IO ()
runPipeline config = case fileInputPath config of
                        "" -> runUdpQuotePipeline config
                        _  -> runFilePipeline config
