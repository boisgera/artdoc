#!/usr/bin/env runhaskell

import Data.Map (empty, lookup, Map(..), toList)
import System.Console.GetOpt
import System.Environment (getArgs)
import Text.JSON
import Text.Pandoc
--------------------------------------------------------------------------------
-- TODO: it would be nice to support custom writer options via the command-line,
--       but this feature of pandoc is only present in the script, not the 
--       library. I don't feel like cut-and-pasting this huge chunk of logic
--       here.
-- TODO: reader options ? such as smart "-" ?
--------------------------------------------------------------------------------
data Opt = Opt { optWriter :: String }

defaultOpts :: Opt
defaultOpts = Opt { optWriter = "html" }

optSpec :: [OptDescr (Opt -> IO Opt)]
optSpec =     
  [ 
    Option "tw" ["to","write"]
      (ReqArg (\arg opt -> return opt { optWriter = arg }) "FORMAT")
      ""
  ]

--------------------------------------------------------------------------------
type StringWriter = Pandoc -> String

-- Rk: those two wraps would probably be a job for typeclasses ...
wrapBlocks :: [Block] -> Pandoc
wrapBlocks blocks = Pandoc noMeta blocks
  where noMeta = Meta {unMeta = empty}

wrapInlines :: [Inline] -> Pandoc
wrapInlines inlines = wrapBlocks [Plain(inlines)]

toJSON :: MetaValue -> StringWriter -> JSValue
toJSON (MetaMap map) writer = JSObject (toJSObject (Prelude.map convert (toList map)))
  where convert (k, v) = (k, toJSON v writer)  
toJSON (MetaList values) writer = JSArray (Prelude.map (\v -> toJSON v writer) values)
toJSON (MetaBool bool) writer = JSBool bool
toJSON (MetaString string) _ = JSString (toJSString string)
toJSON (MetaInlines inlines) writer = JSString (toJSString string)
  where string = writer (wrapInlines inlines)
toJSON (MetaBlocks blocks) writer = JSString (toJSString string)
  where string = writer (wrapBlocks blocks)

jsConvert :: Pandoc -> StringWriter -> JSValue
jsConvert (Pandoc Meta{unMeta=meta} blocks) writer = toJSON (MetaMap meta) writer 

writerOptions :: WriterOptions -- hard-coded :(
writerOptions = def {writerEmailObfuscation = NoObfuscation}

main :: IO ()
main = do  
       rawArgs <- getArgs
       let (actions, args, errors) =  getOpt Permute optSpec rawArgs

       let filename = (args !! 0)
       txt <- readFile filename

       let maybeDoc = readMarkdown def txt
       let doc = case maybeDoc of (Left pandocError) -> error (show pandocError)
                                  (Right doc') -> doc'

       -- Cargo cult 
       opts <- foldl (>>=) (return defaultOpts) actions
       let Opt { optWriter = writerName } = opts

       let writerOrError = getWriter writerName
       case writerOrError of
          Left errorMessage -> error errorMessage
          Right writerMix -> let 
              PureStringWriter writerFactory = writerMix
              writer = writerFactory writerOptions
              jsValue = jsConvert doc writer
              string = Text.JSON.encode jsValue
            in
              putStrLn string

