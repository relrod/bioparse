{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Bio.Parse.Sequence.Fasta where

import Bio.Core.Sequence
import Bio.Parse.Sequence.SequenceParser
import Control.Applicative
import Control.Lens
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Typeable
import Text.Parser.Char
import Text.Parser.Combinators

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
parseHeader :: A.Parser [BL.ByteString]
parseHeader = do
  _ <- char '>'
  fmap BL.pack <$> manyTill anyChar newline `sepBy1` char '|'

-- | Parses an individual line of the sequence.
parseSequenceLine :: A.Parser BL.ByteString
parseSequenceLine = do
  nucleotides <- some . choice $ [letter, char '*', char '-']
  _ <- newline
  return . BL.pack $ nucleotides

-- | Parses an entire sequence including its header.
parseSequence :: A.Parser FastaSequence
parseSequence = do
  h <- parseHeader
  nucleotides <- some parseSequenceLine
  return $ FastaSequence h (BL.concat nucleotides)

-- | Parses many sequences.
parseSequences :: A.Parser [FastaSequence]
parseSequences = manyTill parseSequence eof

-- | Parses sequences from a 'String'.
--
--    @parseFasta = 'parseFastaB' . 'B.pack' x@
parseFasta :: String -> A.Result [FastaSequence]
parseFasta = parseFastaB . B.pack

-- | Parses sequences from a strict 'Data.ByteString.ByteString'.
--
--    @parseFastaB x = 'parseByteString' 'parseSequences' x@
parseFastaB :: B.ByteString -> A.Result [FastaSequence]
parseFastaB = A.parse parseSequences

instance SequenceParser FastaSequence where
  parseString     = parseFasta
  parseByteString = parseFastaB
