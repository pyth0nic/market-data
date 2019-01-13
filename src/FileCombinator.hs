module FileCombinator where

import           Conduit
import           Control.Monad   (unless)
import qualified Data.ByteString as S
import           GHC.Word

accumUntilChar :: Monad m => GHC.Word.Word8 -> ConduitT S.ByteString S.ByteString m ()
accumUntilChar charNum =
    loop []
    where
    loop acc = await >>= maybe (finish acc) (go acc)

    finish acc =
        let final = S.concat $ reverse acc
            in unless (S.null final) (yield final)

    go acc more =
        case S.uncons second of
            Just (_, second') -> yield (S.concat $ reverse $ first:acc) >> go [] second'
            Nothing -> loop $ more:acc
        where
        (first, second) = S.break (== charNum) more
