module Text.Delimited.Types (
	Content, Record, Field
) where

import Data.ByteString (ByteString)

-- | A delimited file is a series of variable length records.
type Content = [Record]

-- | A record is a series of fields.
-- Each record is located on a separate line, delimited by a line break (CRLF).
type Record = [Field]

-- | A field is a strict ByteString.
type Field = ByteString
