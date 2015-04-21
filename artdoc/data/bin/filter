#!/usr/bin/env runhaskell

import Text.Pandoc
import Text.Pandoc.JSON

to_html :: Block -> Block
to_html (RawBlock (Format "latex") string)  = RawBlock (Format "html") string
to_html x = x

main :: IO ()
main = toJSONFilter to_html

