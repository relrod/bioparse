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
    _header :: [BL.ByteString]
  , _data   :: BL.ByteString
  } deriving (Eq, Ord, Show, Typeable)

makeLenses ''FastaSequence

instance BioSeq FastaSequence where
  seqid     = SeqLabel . head . _header
  seqheader = SeqLabel . BL.concat . _header
  seqdata   = SeqData . _data
  seqlength = fromIntegral . BL.length . _data

parseHeader :: Parser [BL.ByteString]
parseHeader = do
  _ <- char '>'
  fmap BL.pack <$> manyTill anyChar newline `sepBy1` char '|'

parseSequenceLine :: Parser BL.ByteString
parseSequenceLine = do
  nucleotides <- many . choice $ [letter, char '*', char '-']
  _ <- newline
  return . BL.pack $ nucleotides

parseSequence :: Parser FastaSequence
parseSequence = do
  h <- parseHeader
  nucleotides <- many parseSequenceLine
  return (FastaSequence h (BL.concat nucleotides))

parseSequences :: Parser [FastaSequence]
parseSequences = manyTill parseSequence eof

parseFasta :: String -> Result [FastaSequence]
parseFasta = parseString parseSequences mempty
