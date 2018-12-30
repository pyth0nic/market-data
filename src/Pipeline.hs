module Pipeline where
    
import           Network
import           Network.Socket                    (PortNumber)
import           Data.Conduit                      (runConduitRes)
import           Data.Conduit.List                 (chunksOf)
import           Conduit
import           Data.Text.Encoding                (encodeUtf8)
import           Parser.Quote                      (printer
                                                   ,sortByTradingTime
                                                   ,Quote(..))
import qualified Data.ByteString                   as B
import qualified Data.Text                         as T

packStr' :: String -> B.ByteString
packStr' = encodeUtf8 . T.pack

-- Everything passes through but sink filters out Nothing values
left :: Either l r -> Maybe l
left = either Just (const Nothing)

right :: Either l r -> Maybe r
right = either (const Nothing) Just

parseResultSplitter :: Monad m => Bool -> ConduitT (Either String Quote) (Either B.ByteString B.ByteString) m ()
parseResultSplitter reorder = getZipConduit
    $ ZipConduit (concatMapC left .| mapC (\x -> packStr' $ (show x) ++ "\n") -- todo add raw data to error message
                                     .| mapC Left)

   *> ZipConduit (concatMapC right .| maybeSortAndPrint reorder .| mapC Right)

maybeSortAndPrint :: Monad m => Bool -> ConduitT Quote B.ByteString m ()
maybeSortAndPrint True  = sortAndPrint 
maybeSortAndPrint False = mapC (\x -> packStr' $ (show x) ++ "\n")

-- In this data set there is the potential of up to aound 20 out of order entries based on window size of 900
-- TODO to solve this dechunk and then chunk to pairs and sort
-- TODO chunksOf as a timewindow would be nice
sortAndPrint :: Monad m => ConduitT Quote B.ByteString m ()
sortAndPrint = chunksOf 900 -- about 300 events/second, (3 second ordering window)
                .| mapC sortByTradingTime
                    .| mapC (\x -> packStr' $ (concat $ map (\x' -> (show x') ++ "\n") x))

showToFile :: ConduitT (Either B.ByteString B.ByteString) Void (ResourceT IO) ()
showToFile = getZipConduit
               $  ZipConduit (concatMapC left .| sinkFile "error.txt")
               *> ZipConduit (concatMapC right .| sinkFile "results.txt")

toFilePipeline :: Bool -> ConduitM (B.ByteString, b) Void (ResourceT IO) ()
toFilePipeline reorder = mapC fst
                    .| mapC printer
                        .| parseResultSplitter reorder
                            .| showToFile

-- todo toKafkaPipeline

portNumber :: Integer -> PortNumber
portNumber port = read (show port) :: PortNumber

runUdpQuotePipeline :: String -> Integer -> Bool -> IO ()
runUdpQuotePipeline addr port reorder = do
                                  runConduitRes $ udpSource addr (portNumber port) 500 .| toFilePipeline reorder