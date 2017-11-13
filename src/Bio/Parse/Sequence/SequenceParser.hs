{-# LANGUAGE ConstraintKinds #-}
module Bio.Parse.Sequence.SequenceParser where

import qualified Data.ByteString.Char8 as B

import Text.Parser.Char
import Text.Parser.Token

type ParseConstraint m = (Monad m, CharParsing m, TokenParsing m)

-- | A general way to talk about \"sequences which can be parsed\".
--
-- In a previous life, this talked about a specific parsing function, binding us
-- to a specific parser combinator library, making our rendering our use of
-- @parsers@ pointless. Now we solely rely on 'CharParsing' (from @parsers@)
-- and leave what to do with that fact up to the call site.
class SequenceParser a where
  parseSequence :: ParseConstraint m => m a
  parseSequences :: ParseConstraint m => m [a]
