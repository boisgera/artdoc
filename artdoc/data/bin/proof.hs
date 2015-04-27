#!/usr/bin/env runhaskell

import Text.Pandoc
import Text.Pandoc.JSON
import Data.List
import Debug.Trace

-- Shit, the design is wrong, the end delimiter may be in another block ...
-- We may have to MERGE blocks, not only split them ! :(

start :: Inline -> Bool
start (Strong ((Str string):_)) = isPrefixOf "Proof" string
start inline = False

end :: Inline -> Bool
end inline@(Math InlineMath "\\blacksquare") = trace ("***** " ++ show inline) True
end inline = trace ("----- " ++ show inline) False

match_end :: ([Inline], [Inline]) -> ([Inline], [Inline])
match_end (is, j:js)
  | end j = (is ++ [j], js)
  | otherwise = match_end (is ++ [j], js)
match_end (is, []) = error "no end marker found."

split :: [Inline] -> [Block]
split inlines = snd (split_step (inlines, []))

append :: Block -> Inline -> Block
append (Para inlines) inline = Para (inlines ++ [inline])
append (Plain inlines) inline = Plain (inlines ++ [inline])
append x _ = error "can only append inline to Para or Plain"

split_step :: ([Inline], [Block]) -> ([Inline], [Block])
split_step (inline:inlines, blocks) = 
    if start inline then
        let (match, remain) = match_end ([], inline:inlines)
        in (remain, blocks ++ [Div ("", ["section", "proof"], []) [Para(match)]])
    else 
        if null blocks then
            split_step (inlines, [Para [inline]]) 
        else 
            split_step (inlines, (init blocks) ++ [last_block])
                where last_block = append (last blocks) inline
split_step ([], blocks) = ([], blocks)

wrap_block :: Block -> [Block]
wrap_block (Para inlines) = split inlines 
wrap_block (Plain inlines) = split inlines
wrap_block block = [block]

wrap_blocks :: [Block] -> [Block]
wrap_blocks (block:blocks) =  (wrap_block block) ++ (wrap_blocks blocks) 
wrap_blocks [] = [] 

divProof :: Pandoc -> Pandoc
divProof (Pandoc metadata blocks) = Pandoc metadata (wrap_blocks blocks)

main :: IO ()
main = toJSONFilter divProof

