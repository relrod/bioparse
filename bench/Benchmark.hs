module Main where

import Bio.Parse.Sequence.SequenceParser
import qualified Bio.Parse.Sequence.Fasta as Fasta
import qualified Bio.Parse.Sequence.FastQ as FastQ
import qualified Bio.Parse.Sequence.Phd as Phd
import qualified Bio.Sequence.Fasta as Biofasta
import qualified Bio.Sequence.FastQ as BiofastQ
import Criterion.Main
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString.Char8 as B

setupEnv :: IO (B.ByteString, B.ByteString, B.ByteString)
setupEnv = do
  ebola'bs <- B.readFile "bench/ebola.fa"
  testfq'bs <- B.readFile "bench/test.fq"
  testphd'bs <- B.readFile "bench/test.phd"
  return (ebola'bs, testfq'bs, testphd'bs)

fastaParseAtto :: B.ByteString -> Either String Fasta.FastaSequence
fastaParseAtto = parseOnly parseSequence

phdParseAtto :: B.ByteString -> Either String Phd.PhdSequence
phdParseAtto = parseOnly parseSequence

fastqParseAtto :: B.ByteString -> Either String FastQ.FastQSequence
fastqParseAtto = parseOnly parseSequence

main :: IO ()
main =
  defaultMain [
    env setupEnv $ \ ~(ebola'bs, _, _) ->
      bgroup "FASTA/bioparse"
      [
        bench "Bio.Parse.Sequence.Fasta attoparsec" $ whnf fastaParseAtto ebola'bs
      ]
    , bgroup "FASTA/biofasta"
     [
       bench "Bio.Sequence.Fasta.readFasta" $ whnf Biofasta.readFasta "bench/ebola.fa"
     ]
  , env setupEnv $ \ ~(_, testfq'bs, _) ->
      bgroup "FASTQ/bioparse"
     [
       bench "Bio.Parse.Sequence.FastQ attoparsec" $ whnf fastqParseAtto testfq'bs
     ]
  , bgroup "FASTQ/biofastq"
    [
       bench "Bio.Sequence.FastQ.readSangerQ" $ whnf BiofastQ.readSangerQ "bench/test.fq"
    ]
  , env setupEnv $ \ ~(_, _, testphd'bs) ->
      bgroup "PHD/bioparse"
     [
       bench "Bio.Parse.Sequence.Phd attoparsec" $ whnf phdParseAtto testphd'bs
     ]
  ]
