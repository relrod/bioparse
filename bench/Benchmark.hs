module Main where

import qualified Bio.Parse.Sequence.Fasta as Fasta
import qualified Bio.Sequence.Fasta as Biofasta
import Criterion.Main

setupEnv :: IO String
setupEnv =
  readFile "bench/ebola.fa"

main :: IO ()
main =
  defaultMain [
    env setupEnv $ \ ~fasta ->
      bgroup "FASTA/bioparse"
      [
        bench "Bio.Parse.Sequence.Fasta.parseFasta" $ whnf Fasta.parseFasta fasta
      ]
    , bgroup "FASTA/biofasta"
     [
       bench "Bio.Sequence.Fasta.readFasta" $ whnf Biofasta.readFasta "bench/ebola.fa"
     ]

  ]
