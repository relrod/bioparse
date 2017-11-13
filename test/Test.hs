module Main where

import qualified Bio.Parse.Sequence.Fasta as Fasta
import Test.Tasty
import Test.Tasty.HUnit
import Text.Trifecta

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [fastaUnitTests]

fastaTest = ">KM034562v1\n\
             \CGGACACACAAAAAGAAAGAAGAATTTTTAGGATCTTTTGTGTGCGAATA\n\
             \ACTATGAGGAAGATTAATAATTTTCCTCTCATTGAAATTTATATCGGAAT\n\
             \TTAAATTGAAATTGTTACTGTAATCATACCTGGTTTGTTTCAGAGCCATA\n\
             \TCACCAAGATAGAGAACAACCTAGGTCTCCGGAGGGGGCAAGGGCATCAG\n"


fastaUnitTests = testGroup "FASTA Unit tests"
  [ testCase "Parses a small FASTA file successfully" $
      successfulParse (Fasta.parseFasta fastaTest)
  ]

successfulParse :: Result [Fasta.FastaSequence] -> Assertion
successfulParse (Success _) = return ()
successfulParse (Failure f) = assertFailure $ "Unsuccessful parse: " ++ show f
