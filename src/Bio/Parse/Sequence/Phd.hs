{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Bio.Parse.Sequence.Phd where

import Bio.Core.Sequence
import Bio.Parse.Sequence.SequenceParser
import Control.Applicative
import Control.Lens
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Monoid (mempty)
import Data.Typeable
import Text.Trifecta as T

data Nucleotide = Nucleotide {
    _nucleotide :: Char
  , _quality    :: Integer
  , _traceIdx   :: Integer
  } deriving (Eq, Ord, Show, Typeable)

makeLenses ''Nucleotide

data PhdSequence = PhdSequence {
    _identifier :: BL.ByteString
  , _sequence   :: [Nucleotide]
  } deriving (Eq, Ord, Show, Typeable)

makeLenses ''PhdSequence

instance BioSeq PhdSequence where
  seqid     = SeqLabel . _identifier
  seqheader = SeqLabel . _identifier
  seqdata   = SeqData . BL.pack . map _nucleotide . _sequence
  seqlength = fromIntegral . length . _sequence

-- | Parse a comment from a PHD file.
parseComment :: Parser BL.ByteString
parseComment = do
  _ <- string "BEGIN_COMMENT"
  BL.pack <$> manyTill anyChar (try (string "END_COMMENT"))

-- | Parse a nucleotide line from a PHD file.
parseNucleotide :: Parser Nucleotide
parseNucleotide = do
  nucleotide' <- oneOf "acgt"   -- TODO: Is uppercase valid too?
  spaces
  quality' <- integer
  spaces
  traceIdx' <- integer
  _ <- newline
  return $ Nucleotide nucleotide' quality' traceIdx'

-- | Parse a section of nucleotides from a PHD file.
parseDnaSection :: Parser [Nucleotide]
parseDnaSection = do
  _ <- string "BEGIN_DNA"
  _ <- newline
  nucleotides <- many parseNucleotide
  _ <- string "END_DNA"
  return nucleotides

-- | Parse a sequence from a PHD file.
parseSequence :: Parser PhdSequence
parseSequence = do
  _ <- string "BEGIN_SEQUENCE"
  spaces
  identifier' <- BL.pack <$> manyTill anyChar (try newline)
  sequence' <- parseDnaSection
  _ <- string "END_SEQUENCE"
  return $ PhdSequence identifier' sequence'

-- | Parses many sequences.
parseSequences :: Parser [PhdSequence]
parseSequences = manyTill parseSequence eof

-- | Parses sequences from a 'String'.
--
--    @parsePhd x = 'parseString' 'parseSequences' 'mempty' x@
parsePhd :: String -> Result [PhdSequence]
parsePhd = T.parseString parseSequences mempty

-- | Parses sequences from a strict 'Data.ByteString.ByteString'.
--
--    @parseFastaB x = 'parseByteString' 'parseSequences' 'mempty' x@
parsePhdB :: B.ByteString -> Result [PhdSequence]
parsePhdB = T.parseByteString parseSequences mempty

instance SequenceParser PhdSequence where
  parseString     = parsePhd
  parseByteString = parsePhdB
