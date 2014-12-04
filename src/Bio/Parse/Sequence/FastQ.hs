{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Bio.Parse.Sequence.FastQ where

import Bio.Core.Sequence
import Bio.Parse.Sequence.SequenceParser
import Control.Applicative
import Control.Lens
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List
import Data.Typeable
import Text.Parser.Char
import Text.Parser.Combinators


data FastQSequence = FastQSequence {
    _header   :: [BL.ByteString]
  , _sequence :: BL.ByteString
  , _quality  :: BL.ByteString
  } deriving (Eq, Ord, Show, Typeable)

makeLenses ''FastQSequence

instance BioSeq FastQSequence where
  seqid     = SeqLabel . head . _header
  seqheader = SeqLabel . BL.concat . intersperse (BL.pack ":") . _header
  seqdata   = SeqData . _sequence
  seqlength = fromIntegral . BL.length . _sequence

instance BioSeqQual FastQSequence where
  seqqual = QualData . _quality

-- | Parses the header of a FASTQ file.
--
-- The header starts with a @\@@ character and follows with an optional
-- description. When making use if 'Bio.Core.BioSeq', the first word of the
-- header is used as the 'seqid'.
parseHeader :: A.Parser [BL.ByteString]
parseHeader = do
  _ <- char '@'
  fmap BL.pack <$> manyTill anyChar newline `sepBy1` char ':'

-- | Parses the line of sequence data.
parseSequenceLine :: A.Parser BL.ByteString
parseSequenceLine = do
  nucleotides <- some letter
  return . BL.pack $ nucleotides

-- | Parses the line of quality data.
parseQualityLine :: A.Parser BL.ByteString
parseQualityLine = do
  qualityChars <- manyTill anyChar newline
  return . BL.pack $ qualityChars

-- | Parses an entire sequence including its header and quality data.
parseSequence :: A.Parser FastQSequence
parseSequence = do
  h <- parseHeader
  nucleotides <- parseSequenceLine
  _ <- newline
  _ <- char '+'
  _ <- manyTill anyChar newline
  qualityChars <- parseQualityLine
  return (FastQSequence h nucleotides qualityChars)

-- | Parses many sequences.
parseSequences :: A.Parser [FastQSequence]
parseSequences = manyTill parseSequence eof

-- | Parses sequences from a 'String'.
--
--    @parseFastQ = 'parseFastQB' . 'B.pack'@
parseFastQ :: String -> A.Result [FastQSequence]
parseFastQ = parseFastQB . B.pack

-- | Parses sequences from a strict 'Data.ByteString.ByteString'.
--
--    @parseFastQB x = 'A.parse' 'parseSequences' x@
parseFastQB :: B.ByteString -> A.Result [FastQSequence]
parseFastQB = A.parse parseSequences

instance SequenceParser FastQSequence where
  parseString     = parseFastQ
  parseByteString = parseFastQB
