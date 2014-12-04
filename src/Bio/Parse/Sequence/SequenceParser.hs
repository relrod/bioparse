module Bio.Parse.Sequence.SequenceParser where

import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString as A

class SequenceParser m where
  parseString :: String -> A.Result [m]
  parseByteString :: B.ByteString -> A.Result [m]
