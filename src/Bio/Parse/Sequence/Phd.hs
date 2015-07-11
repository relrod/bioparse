{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Bio.Parse.Sequence.Phd where

import Bio.Parse.Sequence.SequenceParser
import Control.Applicative
import Control.Lens
import Control.Monad
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

-- | Parse a comment from a PHD file.
--
-- We store the comment as a 'BL.ByteString' but we don't actually use it right
-- now.
parseComment :: ParseConstraint m => m BL.ByteString
parseComment = do
  _ <- string "BEGIN_COMMENT"
  BL.pack <$> manyTill anyChar (try (string "END_COMMENT"))

-- | Treat comments as spaces.
spacesWithComments :: ParseConstraint m => m ()
spacesWithComments = skipSome $ choice [void (some space), void parseComment]

-- | Parse a nucleotide line from a PHD file.
parseNucleotide :: (TokenParsing m, ParseConstraint m) => m Nucleotide
parseNucleotide = do
  nucleotide' <- letter
  spaces
  quality' <- natural
  spaces
  traceIdx' <- natural
  _ <- many newline
  return $ Nucleotide nucleotide' quality' traceIdx'

-- | Parse a section of nucleotides from a PHD file.
parseDnaSection :: (TokenParsing m, ParseConstraint m) => m [Nucleotide]
parseDnaSection = do
  _ <- string "BEGIN_DNA"
  _ <- some newline
  nucleotides <- manyTill parseNucleotide (try (string "END_DNA"))
  _ <- some newline
  return nucleotides

-- | Parse a sequence from a PHD file.
parseSequence :: (TokenParsing m, ParseConstraint m) => m PhdSequence
parseSequence = do
  _ <- string "BEGIN_SEQUENCE "
  identifier' <- BL.pack <$> manyTill anyChar (try newline)
  _ <- some newline
  sequence' <- parseDnaSection
  _ <- string "END_SEQUENCE"
  _ <- many newline
  return $ PhdSequence identifier' sequence'

-- | Parses many sequences.
parseSequences :: (TokenParsing m, ParseConstraint m) => m [PhdSequence]
parseSequences = manyTill (spacesWithComments *> parseSequence <* spacesWithComments) eof

-- | Parses sequences from a 'String'.
--
--    @parsePhd = 'parsePhdB' . 'B.pack'@
parsePhd :: String -> Either String [PhdSequence]
parsePhd = parsePhdB . B.pack

-- | Parses sequences from a strict 'Data.ByteString.ByteString'.
--
--    @parsePhdB x = 'A.parseOnly' 'parseSequences' x@
parsePhdB :: B.ByteString -> Either String [PhdSequence]
parsePhdB = A.parseOnly parseSequences

instance SequenceParser PhdSequence where
  parseString     = parsePhd
  parseByteString = parsePhdB
