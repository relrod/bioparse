{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Bio.Parse.Sequence.Phd where

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
import Text.Parser.Token

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
parseComment :: A.Parser BL.ByteString
parseComment = do
  _ <- string "BEGIN_COMMENT"
  BL.pack <$> manyTill anyChar (try (string "END_COMMENT"))

-- | Parse a nucleotide line from a PHD file.
parseNucleotide :: A.Parser Nucleotide
parseNucleotide = do
  nucleotide' <- letter
  spaces
  quality' <- natural
  spaces
  traceIdx' <- natural
  _ <- many newline
  return $ Nucleotide nucleotide' quality' traceIdx'

-- | Parse a section of nucleotides from a PHD file.
parseDnaSection :: A.Parser [Nucleotide]
parseDnaSection = do
  _ <- string "BEGIN_DNA"
  _ <- some newline
  nucleotides <- manyTill parseNucleotide (try (string "END_DNA"))
  _ <- some newline
  return nucleotides

-- | Parse a sequence from a PHD file.
parseSequence :: A.Parser PhdSequence
parseSequence = do
  _ <- string "BEGIN_SEQUENCE "
  identifier' <- BL.pack <$> manyTill anyChar (try newline)
  _ <- some newline
  sequence' <- parseDnaSection
  _ <- string "END_SEQUENCE"
  _ <- many newline
  return $ PhdSequence identifier' sequence'

-- | Parses many sequences.
parseSequences :: A.Parser [PhdSequence]
parseSequences = manyTill parseSequence eof

-- | Parses sequences from a 'String'.
--
--    @parsePhd x = 'parseString' 'parseSequences' 'mempty' x@
parsePhd :: String -> A.Result [PhdSequence]
parsePhd = parsePhdB . B.pack

-- | Parses sequences from a strict 'Data.ByteString.ByteString'.
--
--    @parseFastaB x = 'parseByteString' 'parseSequences' 'mempty' x@
parsePhdB :: B.ByteString -> A.Result [PhdSequence]
parsePhdB = A.parse parseSequences

instance SequenceParser PhdSequence where
  parseString     = parsePhd
  parseByteString = parsePhdB
