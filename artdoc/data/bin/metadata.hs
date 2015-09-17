#!/usr/bin/env runhaskell

import Data.Map
import Data.Maybe
import System.IO
import System.Environment
import Text.JSON
import Text.Pandoc
import Text.Pandoc.JSON

-- TODO: output type (writer name) for fields that contain some Pandoc content.
-- TODO: need to specify HTML writer options (namely: set email obfuscation to none)

type StringWriter = WriterOptions -> Pandoc -> String

toJSON :: MetaValue -> StringWriter -> JSValue
toJSON (MetaMap map) writer = JSObject (toJSObject (Prelude.map convert (toList map)))
  where convert (k, v) = (k, toJSON v writer)  
toJSON (MetaList values) writer = JSArray (Prelude.map (\v -> toJSON v writer) values)
toJSON (MetaBool bool) writer = JSBool bool
toJSON (MetaString string) _ = JSString (toJSString string)
toJSON (MetaInlines inlines) writer = JSString (toJSString string)
  where string = writer def (wrapInlines inlines)
toJSON (MetaBlocks blocks) writer = JSString (toJSString string)
  where string = writer def (wrapBlocks blocks)

jsConvert :: Pandoc -> StringWriter -> JSValue
jsConvert (Pandoc Meta{unMeta=meta} blocks) writer = toJSON (MetaMap meta) writer 

getFromMeta :: Pandoc -> String -> MetaValue
getFromMeta (Pandoc meta blocks) key = fromJust (Data.Map.lookup key yaml)
  where yaml = unMeta meta

wrapBlocks :: [Block] -> Pandoc
wrapBlocks blocks = Pandoc noMeta blocks
  where noMeta = Meta {unMeta = Data.Map.empty}

wrapInlines :: [Inline] -> Pandoc
wrapInlines inlines = wrapBlocks [Plain(inlines)]

customWriteHtmlString :: StringWriter
customWriteHtmlString options doc = let
    options' = options {writerEmailObfuscation = NoObfuscation}
  in
    writeHtmlString options' doc

main :: IO ()
main = do  
    args <- getArgs
    let filename = (args !! 0)
    txt <- readFile filename
    let maybeDoc = readMarkdown def txt
    case maybeDoc of
      Left pandocError -> error (show pandocError)
      Right doc -> putStrLn (encode (jsConvert doc customWriteHtmlString))

