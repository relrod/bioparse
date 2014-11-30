module Main where

import qualified Bio.Parse.Sequence.Fasta as Fasta
import qualified Bio.Sequence.Fasta as Biofasta
import Criterion.Main
import qualified Data.ByteString.Char8 as B

setupEnv :: IO (String, B.ByteString)
setupEnv = do
  ebola's <- readFile "bench/ebola.fa"
  ebola'bs <- B.readFile "bench/ebola.fa"
  return (ebola's, ebola'bs)

main :: IO ()
main =
  defaultMain [
    env setupEnv $ \ ~(ebola's, ebola'bs) ->
      bgroup "FASTA/bioparse/parseFasta"
      [
        bench "Bio.Parse.Sequence.Fasta.parseFasta" $ whnf Fasta.parseFasta ebola's
      , bench "Bio.Parse.Sequence.Fasta.parseFastaB" $ whnf Fasta.parseFastaB ebola'bs
      ]
    , bgroup "FASTA/biofasta"
     [
       bench "Bio.Sequence.Fasta.readFasta" $ whnf Biofasta.readFasta "bench/ebola.fa"
     ]

  ]
