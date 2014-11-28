{-# LANGUAGE TemplateHaskell #-}
module Bio.Parse.Sequence.Fasta where

import Bio.Core.Sequence
import Control.Lens
import qualified Data.ByteString.Lazy.Char8 as BL
import Text.Trifecta

data FastaSequence = FastaSequence {
    _id     :: BL.ByteString
  , _header :: BL.ByteString
  , _data   :: BL.ByteString
  } deriving (Eq, Ord, Show)

makeLenses ''FastaSequence

instance BioSeq FastaSequence where
  seqid = SeqLabel . _id
  seqheader = SeqLabel . _header
  seqdata = SeqData . _data
  seqlength = fromIntegral . BL.length . _data

--parseHeader :: Parser BL.ByteString
--parseHeader = do
--  error "TODO"
