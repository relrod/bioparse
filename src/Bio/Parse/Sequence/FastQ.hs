{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Bio.Parse.Sequence.FastQ where

import Bio.Core.Sequence
import Control.Applicative
import Control.Lens
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List
import Data.Monoid (mempty)
import Data.Typeable
import Text.Trifecta

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
parseHeader :: Parser [BL.ByteString]
parseHeader = do
  _ <- char '@'
  fmap BL.pack <$> manyTill anyChar newline `sepBy1` char ':'

-- | Parses the line of sequence data.
parseSequenceLine :: Parser BL.ByteString
parseSequenceLine = do
  nucleotides <- some letter
  return . BL.pack $ nucleotides

-- | Parses the line of quality data.
parseQualityLine :: Parser BL.ByteString
parseQualityLine = do
  qualityChars <- manyTill anyChar newline
  return . BL.pack $ qualityChars

-- | Parses an entire sequence including its header and quality data.
parseSequence :: Parser FastQSequence
parseSequence = do
  h <- parseHeader
  nucleotides <- parseSequenceLine
  _ <- newline
  _ <- char '+'  -- TODO: The header might be repeated here.
  _ <- newline
  qualityChars <- parseQualityLine
  return (FastQSequence h nucleotides qualityChars)

-- | Parses many sequences.
parseSequences :: Parser [FastQSequence]
parseSequences = manyTill parseSequence eof

-- | Parses sequences from a 'String'.
--
--    @parseFastQ x = 'parseString' 'parseSequences' 'mempty' x@
parseFastQ :: String -> Result [FastQSequence]
parseFastQ = parseString parseSequences mempty

-- | Parses sequences from a strict 'Data.ByteString.ByteString'.
--
--    @parseFastQB x = 'parseByteString' 'parseSequences' 'mempty' x@
parseFastQB :: B.ByteString -> Result [FastQSequence]
parseFastQB = parseByteString parseSequences mempty
