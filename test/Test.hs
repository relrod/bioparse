module Main where

import Bio.Parse.Sequence.SequenceParser
import qualified Bio.Parse.Sequence.Fasta as Fasta
import qualified Bio.Parse.Sequence.FastQ as FastQ
import qualified Bio.Parse.Sequence.Phd as Phd
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString.Char8 as B
import Test.Tasty
import Test.Tasty.HUnit
import Text.Trifecta

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [fastaTrifectaTests, fastaAttoparsecTests]

-- | Should be a successful parse.
fastaTestSuccess :: B.ByteString
fastaTestSuccess = B.pack ">KM034562v1\n\
                          \CGGACACACAAAAAGAAAGAAGAATTTTTAGGATCTTTTGTGTGCGAATA\n\
                          \ACTATGAGGAAGATTAATAATTTTCCTCTCATTGAAATTTATATCGGAAT\n\
                          \TTAAATTGAAATTGTTACTGTAATCATACCTGGTTTGTTTCAGAGCCATA\n\
                          \TCACCAAGATAGAGAACAACCTAGGTCTCCGGAGGGGGCAAGGGCATCAG\n"

-- | Should be a successful parse.
fastaMultiSuccess :: B.ByteString
fastaMultiSuccess = B.pack ">foo\nABCDEFG\nFOOBAR\n>bar\nHELLO\nWORLD\n"

-- | Should be a failed parse.
fastaSingleFail :: B.ByteString
fastaSingleFail = B.pack ">foo\nABCDEFG\nFOOB4R\n"

-- | Should be a failed parse.
fastaMultiFail :: B.ByteString
fastaMultiFail = B.pack ">bar\nBANANA\nFISHIES\n>foo\nABCDEFG\nFOOB4R\n"

--------------------------------------------------------------------------------

-- TODO: Deduplicate all of these...
fastaTrifectaTests = testGroup "FASTA (trifecta)"
  [ testCase "Parses a small FASTA file successfully" $
      trifectaSuccessfulParse (trifectaFASTASingle fastaTestSuccess)
  , testCase "Parses a small multi-FASTA file successfully" $
      trifectaSuccessfulParse (trifectaFASTAMulti fastaMultiSuccess)
  , testCase "Bombs out correctly when a contains an invalid character (single)" $
      trifectaSuccessfullyFailedParse (trifectaFASTASingle fastaSingleFail)
  , testCase "Bombs out correctly when a contains an invalid character (multi)" $
      trifectaSuccessfullyFailedParse (trifectaFASTAMulti fastaMultiFail)
  ]

trifectaSuccessfulParse :: Result a -> Assertion
trifectaSuccessfulParse (Success _) = return ()
trifectaSuccessfulParse (Failure f) = assertFailure $ "Unsuccessful parse: " ++ show f

trifectaSuccessfullyFailedParse :: Result a -> Assertion
trifectaSuccessfullyFailedParse (Success _) = assertFailure $ "Parse succeeded, should have failed"
trifectaSuccessfullyFailedParse (Failure f) = return ()

trifectaFASTASingle :: B.ByteString -> Result Fasta.FastaSequence
trifectaFASTASingle = parseByteString Fasta.parseSequence' mempty

trifectaFASTAMulti :: B.ByteString -> Result [Fasta.FastaSequence]
trifectaFASTAMulti = parseByteString Fasta.parseSequences' mempty

--------------------------------------------------------------------------------

fastaAttoparsecTests = testGroup "FASTA (attoparsec)"
  [ testCase "Parses a small FASTA file successfully" $
      attoSuccessfulParse (attoFASTASingle fastaTestSuccess)
  , testCase "Parses a small multi-FASTA file successfully" $
      attoSuccessfulParse (attoFASTAMulti fastaMultiSuccess)
  , testCase "Bombs out correctly when a contains an invalid character (single)" $
      attoSuccessfullyFailedParse (attoFASTASingle fastaSingleFail)
  , testCase "Bombs out correctly when a contains an invalid character (multi)" $
      attoSuccessfullyFailedParse (attoFASTAMulti fastaMultiFail)
  ]

attoSuccessfulParse :: Either String a -> Assertion
attoSuccessfulParse (Right _) = return ()
attoSuccessfulParse (Left s) = assertFailure $ "Unsuccessful parse: " ++ show s

attoSuccessfullyFailedParse :: Either String a  -> Assertion
attoSuccessfullyFailedParse (Right _) = assertFailure $ "Parse succeeded, should have failed"
attoSuccessfullyFailedParse (Left _) = return ()

attoFASTASingle :: B.ByteString -> Either String Fasta.FastaSequence
attoFASTASingle = parseOnly (Fasta.parseSequence' <* eof)

attoFASTAMulti :: B.ByteString -> Either String [Fasta.FastaSequence]
attoFASTAMulti = parseOnly Fasta.parseSequences'
