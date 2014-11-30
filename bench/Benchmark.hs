module Main where

import qualified Bio.Parse.Sequence.Fasta as Fasta
import qualified Bio.Parse.Sequence.FastQ as FastQ
import qualified Bio.Sequence.Fasta as Biofasta
import qualified Bio.Sequence.FastQ as BiofastQ
import Criterion.Main
import qualified Data.ByteString.Char8 as B

setupEnv :: IO (String, B.ByteString, String, B.ByteString)
setupEnv = do
  ebola's <- readFile "bench/ebola.fa"
  ebola'bs <- B.readFile "bench/ebola.fa"
  testfq's <- readFile "bench/test.fq"
  testfq'bs <- B.readFile "bench/test.fq"
  return (ebola's, ebola'bs, testfq's, testfq'bs)

main :: IO ()
main =
  defaultMain [
    env setupEnv $ \ ~(ebola's, ebola'bs, _, _) ->
      bgroup "FASTA/bioparse"
      [
        bench "Bio.Parse.Sequence.Fasta.parseFasta" $ whnf Fasta.parseFasta ebola's
      , bench "Bio.Parse.Sequence.Fasta.parseFastaB" $ whnf Fasta.parseFastaB ebola'bs
      ]
    , bgroup "FASTA/biofasta"
     [
       bench "Bio.Sequence.Fasta.readFasta" $ whnf Biofasta.readFasta "bench/ebola.fa"
     ]
  , env setupEnv $ \ ~(_, _, testfq's, testfq'bs) ->
      bgroup "FASTQ/bioparse"
     [
        bench "Bio.Parse.Sequence.FastQ.parseFastQ" $ whnf FastQ.parseFastQ testfq's
      , bench "Bio.Parse.Sequence.FastQ.parseFastaQB" $ whnf FastQ.parseFastQB testfq'bs
     ]
  , bgroup "FASTQ/biofastq"
    [
       bench "Bio.Sequence.FastQ.readSangerQ" $ whnf BiofastQ.readSangerQ "bench/test.fq"
     ]
  ]
