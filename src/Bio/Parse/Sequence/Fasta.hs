{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Bio.Parse.Sequence.Fasta where

import Bio.Core.Sequence
import Control.Applicative
import Control.Lens
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Monoid (mempty)
import Data.Typeable
import Text.Trifecta

data FastaSequence = FastaSequence {
    _header   :: [BL.ByteString]
  , _sequence :: BL.ByteString
  } deriving (Eq, Ord, Show, Typeable)

makeLenses ''FastaSequence

instance BioSeq FastaSequence where
  seqid     = SeqLabel . head . _header
  seqheader = SeqLabel . BL.concat . _header
  seqdata   = SeqData . _sequence
  seqlength = fromIntegral . BL.length . _sequence

-- | Parses the header of a FASTA file.
--
-- The header starts with a @>@ character and gets separated by @|@ (pipe)
-- characters. When making use if 'Bio.Core.BioSeq', the first word of the
-- header is used as the 'seqid'.
parseHeader :: Parser [BL.ByteString]
parseHeader = do
  _ <- char '>'
  fmap BL.pack <$> manyTill anyChar newline `sepBy1` char '|'

-- | Parses an individual line of the sequence.
parseSequenceLine :: Parser BL.ByteString
parseSequenceLine = do
  nucleotides <- many . choice $ [letter, char '*', char '-']
  _ <- newline
  return . BL.pack $ nucleotides

-- | Parses an entire sequence including its header.
parseSequence :: Parser FastaSequence
parseSequence = do
  h <- parseHeader
  nucleotides <- many parseSequenceLine
  return (FastaSequence h (BL.concat nucleotides))

-- | Parses many sequences.
parseSequences :: Parser [FastaSequence]
parseSequences = manyTill parseSequence eof

-- | Parses sequences from a 'String'.
--
--    @parseFasta x = 'parseString' 'parseSequences' 'mempty' x@
parseFasta :: String -> Result [FastaSequence]
parseFasta = parseString parseSequences mempty
