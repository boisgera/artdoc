#!/usr/bin/env runhaskell

import Text.Pandoc
import Text.Pandoc.JSON
import Data.List
import Debug.Trace

tag_tombstone :: Inline -> Inline
tag_tombstone tombstone@(Math InlineMath "\\blacksquare") = 
  Span ("", ["tombstone"], []) [tombstone]
tag_tombstone x = x

main :: IO ()
main = toJSONFilter tag_tombstone

