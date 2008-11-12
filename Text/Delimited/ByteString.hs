-- | Parse text files containing lines with records separated by character
-- delimiters.
--
-- At this time parsing is only supported for lazy 'ByteString's.
--
module Text.Delimited.ByteString (
	module Text.Delimited.ByteString.Lazy,
	module Text.Delimited.Types
) where

import Text.Delimited.ByteString.Lazy
import Text.Delimited.Types
