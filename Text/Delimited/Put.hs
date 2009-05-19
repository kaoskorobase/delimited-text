module Text.Delimited.Put (
    putRow
  , putRecord
  , putContent
  , runPut
) where

import Data.Binary.Put          (Put, putByteString, runPut)
import Text.Show.ByteString     (putAscii, showp, unlinesP)
import Text.Delimited.Types

-- | Intersperse a list of 'Put's with a delimiter.
intersperseP :: Put -> [Put] -> Put
intersperseP _ []     = return ()
intersperseP _ (x:[]) = showp x
intersperseP d (x:xs) = x >> d >> intersperseP d xs

-- | Convert 'Record' fields delimited by 'delim' to 'Put'.
putRow :: Char -> [Put] -> Put
putRow delim = intersperseP (putAscii delim)

-- | Convert 'Record' fields delimited by 'delim' to 'Put'.
putRecord :: Char -> Record -> Put
putRecord delim = putRow delim . map putByteString

-- | Convert 'Content' delimited by 'delim' to a 'Put'.
putContent :: Char -> Content -> Put
putContent delim = unlinesP . map (putRecord delim)
