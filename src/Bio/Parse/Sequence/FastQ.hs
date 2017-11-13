{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Bio.Parse.Sequence.FastQ where

import Bio.Parse.Sequence.SequenceParser
import Control.Applicative
import Control.Lens
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Typeable
import Text.Parser.Char
import Text.Parser.Combinators

data FastQSequence = FastQSequence {
    _header   :: [BL.ByteString]
  , _sequence :: BL.ByteString
  , _quality  :: BL.ByteString
  } deriving (Eq, Ord, Show, Typeable)

makeLenses ''FastQSequence

-- | Parses the header of a FASTQ file.
--
-- The header starts with a @\@@ character and follows with an optional
-- description. When making use if 'Bio.Core.BioSeq', the first word of the
-- header is used as the 'seqid'.
parseHeader :: ParseConstraint m => m [BL.ByteString]
parseHeader = do
  _ <- char '@'
  fmap BL.pack <$> manyTill anyChar newline `sepBy1` char ':'

-- | Parses the line of sequence data.
parseSequenceLine :: ParseConstraint m => m BL.ByteString
parseSequenceLine = do
  nucleotides <- some letter
  return . BL.pack $ nucleotides

-- | Parses the line of quality data.
parseQualityLine :: ParseConstraint m => m BL.ByteString
parseQualityLine = do
  qualityChars <- manyTill anyChar newline
  return . BL.pack $ qualityChars

-- | Parses an entire sequence including its header and quality data.
parseSequence' :: ParseConstraint m => m FastQSequence
parseSequence' = do
  h <- parseHeader
  nucleotides <- parseSequenceLine
  _ <- newline
  _ <- char '+'
  _ <- manyTill anyChar newline
  qualityChars <- parseQualityLine
  return (FastQSequence h nucleotides qualityChars)

-- | Parses many sequences.
parseSequences' :: ParseConstraint m => m [FastQSequence]
parseSequences' = manyTill parseSequence eof

instance SequenceParser FastQSequence where
  parseSequence  = parseSequence'
  parseSequences = parseSequences'
