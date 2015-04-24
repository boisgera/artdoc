#!/usr/bin/env runhaskell

import System.Directory
import System.FilePath.Posix
import Text.Pandoc
import Text.Pandoc.JSON

toSVGImage :: String -> IO String
toSVGImage filename = do 
    let svgfilename = replaceExtension filename ".svg"
    svgexists <- doesFileExist svgfilename
    if svgexists 
        then return svgfilename 
        else return filename

toSVG :: Inline -> IO Inline
toSVG (Image inlines (target, title)) = do 
    new_target <- toSVGImage(target)
    return (Image inlines (new_target, title))
toSVG x = return x

main :: IO ()
main = toJSONFilter toSVG

