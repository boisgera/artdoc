#!/usr/bin/env runhaskell

import Data.Map
import Data.Maybe
import System.IO
import System.Environment
import Text.JSON
import Text.Pandoc
import Text.Pandoc.JSON

-- TODO: output type (writer name) for fields that contain some Pandoc content.

toJSON :: MetaValue -> JSValue
toJSON (MetaMap map) = JSObject (toJSObject (Prelude.map convert (toList map)))
  where convert (k, v) = (k, toJSON v)  
toJSON (MetaList values) = JSArray (Prelude.map toJSON values)
toJSON (MetaBool bool) = JSBool bool
toJSON (MetaString string) = JSString (toJSString string)
toJSON (MetaInlines inlines) = JSString (toJSString string)
  where string = writeMarkdown def (wrapInlines inlines)
toJSON (MetaBlocks blocks) = JSString (toJSString string)
  where string = writeMarkdown def (wrapBlocks blocks)

jsConvert :: Pandoc -> JSValue
jsConvert (Pandoc Meta{unMeta=meta} blocks) = toJSON (MetaMap meta) 

getFromMeta :: Pandoc -> String -> MetaValue
getFromMeta (Pandoc meta blocks) key = fromJust (Data.Map.lookup key yaml)
  where yaml = unMeta meta

wrapBlocks :: [Block] -> Pandoc
wrapBlocks blocks = Pandoc noMeta blocks
  where noMeta = Meta {unMeta = Data.Map.empty}

wrapInlines :: [Inline] -> Pandoc
wrapInlines inlines = wrapBlocks [Plain(inlines)]

main :: IO ()
main = do  
    args <- getArgs
    filename <- return (args !! 0)
    txt <- readFile filename
    doc <- return (readMarkdown def txt)
    putStrLn (encode (jsConvert doc))

