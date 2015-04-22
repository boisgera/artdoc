#!/usr/bin/env runhaskell

import System.FilePath.Posix
import Text.Pandoc
import Text.Pandoc.JSON


-- TODO: check that the SVG file exists or don't change the extension.

toSVGImage :: String -> String
toSVGImage filename = replaceExtension filename ".svg"

toSVG :: Inline -> Inline
toSVG (Image inlines (target, title))  = Image inlines (toSVGImage(target), title)
toSVG x = x

main :: IO ()
main = toJSONFilter toSVG

