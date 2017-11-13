{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Bio.Parse.Sequence.Fasta where

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

-- | Parses the header of a FASTA file.
--
-- The header starts with a @>@ character and gets separated by @|@ (pipe)
-- characters. When making use of 'Bio.Core.BioSeq', the first word of the
-- header is used as the 'seqid'.
parseHeader :: ParseConstraint m => m [BL.ByteString]
parseHeader = do
  _ <- char '>'
  fmap BL.pack <$> manyTill anyChar newline `sepBy1` char '|'

-- | Parses an individual line of the sequence.
parseSequenceLine :: ParseConstraint m => m BL.ByteString
parseSequenceLine = do
  nucleotides <- some . choice $ [letter, char '*', char '-']
  _ <- newline
  return . BL.pack $ nucleotides

-- | Parses an entire sequence including its header.
parseSequence :: ParseConstraint m => m FastaSequence
parseSequence = do
  h <- parseHeader
  nucleotides <- some parseSequenceLine
  return $ FastaSequence h (BL.concat nucleotides)

-- | Parses many sequences.
parseSequences :: ParseConstraint m => m [FastaSequence]
parseSequences = manyTill parseSequence eof

-- | Parses sequences from a 'String'.
--
--    @parseFasta = 'parseFastaB' . 'B.pack' x@
parseFasta :: String -> Either String [FastaSequence]
parseFasta = parseFastaB . B.pack

-- | Parses sequences from a strict 'Data.ByteString.ByteString'.
--
--    @parseFastaB x = 'A.parseOnly' 'parseSequences' x@
parseFastaB :: B.ByteString -> Either String [FastaSequence]
parseFastaB = A.parseOnly parseSequences

instance SequenceParser FastaSequence where
  parseString     = parseFasta
  parseByteString = parseFastaB
