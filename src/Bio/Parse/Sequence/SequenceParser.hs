module Bio.Parse.Sequence.SequenceParser where

import qualified Data.ByteString.Char8 as B
import Text.Trifecta

-- | This pretty much emulates the functions exported by 'Text.Trifecta.Parser'.
--
-- In fact, this might be terrible, but I'm trying it. It might go away.
class SequenceParser m where
  parseString :: String -> Result [m]
  parseByteString :: B.ByteString -> Result [m]
