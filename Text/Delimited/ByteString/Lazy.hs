module Text.Delimited.ByteString.Lazy (
    encode, decode, interact
) where

import           Data.ByteString.Lazy (ByteString, toChunks)
import qualified Data.ByteString as BS
import qualified Data.Attoparsec.Char8 as P
import           Prelude hiding (interact)
import           Text.Delimited.Types
import           Text.Delimited.Put (putContent, runPut)

-- | Encode records separated by newlines to a ByteString.
-- Record fields are separated by 'delim'.
encode :: Char -> Content -> ByteString
encode delim = runPut . putContent delim

-- | Construct a strict ByteString from a lazy one.
fromLazy :: ByteString -> BS.ByteString
fromLazy = BS.concat . toChunks

-- | Construct a parser from 'a' terminated by 'b'.
endBy :: P.Parser a -> P.Parser b -> P.Parser a
a `endBy` b = do
    r <- a
    b
    return r

-- | A lazy 'ByteString' parser for delimited text.
parser :: [Char] -> P.Parser Content
parser delims = line `P.manyTill` P.endOfInput
    where
        line  = (field `P.sepBy` sep) `endBy` eol
        field = fromLazy `fmap` P.takeWhile (P.notInClass (delims ++ nls))
        sep   =              P.satisfy $ P.inClass delims
        eol   = P.skipMany1 (P.satisfy $ P.inClass nls)
        nls   = "\n\r"

-- | Parse records separated by newlines from a ByteString.
-- Record fields are separated by any of the characters in 'delims'. There is
-- no way of escaping delimiters, so record fields may not contain any of the
-- characters in 'delims'.
decode :: [Char] -> ByteString -> Result Content
decode delims = snd . P.parse (parser delims)

-- | Decode a ByteString, apply a function to each 'Record' and encode the content.
-- Delimiters may contain multiple characters but only the first is used for
-- encoding.
interact :: (Record -> Record) -> [Char] -> ByteString -> Result ByteString
interact f delims s =
    case decode delims s of
        Right c -> Right (encode (head delims) (map f c))
        Left e  -> Left e
