{-# LANGUAGE ConstraintKinds #-}
module Bio.Parse.Sequence.SequenceParser where

import qualified Data.ByteString.Char8 as B

import Text.Parser.Char

type ParseConstraint m = (Monad m, CharParsing m)

class SequenceParser m where
  parseString :: String -> Either String [m]
  parseByteString :: B.ByteString -> Either String [m]
