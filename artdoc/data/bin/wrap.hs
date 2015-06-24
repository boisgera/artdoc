#!/usr/bin/env runhaskell

import Text.Pandoc
import Text.Pandoc.JSON
import Data.List
import Debug.Trace

-- TODO: wrap stuff that should logically belong to the same paragraph
--       in a div with an appropriate class. This is a wrapping process
--       that takes place at the top-level, in the list of blocks.
--       We consider that *some* blocks don't really create a new
--       paragraph: they are: CodeBlock, RawBlock, Blockquote,
--       all three lists, Table.
--       So, if a paragraph is followed by such as block, group 
--       until you end up with a header or horinzontal rule or paragraph
--       (and when it's a paragraph, include it in the group).

-- Think the stuff a bit more here: we have "true paragraph" and "true blocks".
-- A remark can be made of two paragraphs ... But then again, it may be one
-- paragraph followed by some unnamed paragraph (not in the scope of the 
-- remark). Block would be handy, but the concept is ambiguous :(
-- Rely on sections for them, use headers instead of bold ? Yes, that's
-- probably the way to go ... No, there is the same kind of ambiguity, but
-- (probably badly managed by pandoc). Dunno, think about this stuff.
-- I have to admit that there are some real sources of ambiguities in my
-- documents, such as (free-form) comments after a definition, etc.
-- Force a bold "Comment" or "Remark" tag on them to remove that ambiguity ?
-- (and remove the "Comment" tag from the document ?). Maybe, yes ... I am 
-- sort of already doing it most of the time ... with "Remark".

-- Unless I decide that blocks (or some kind of blocks) cannot be 
-- multi-parapgraph ? Such as Theorem, Definitions, etc ? And ask
-- for an end-marker for multiple-block paragraphs (such as ".." ?) 
-- That would be similar to "proof".
-- Dunno, have a look at the existing examples ...
-- Update: some classes that would be mono-paragraph, that doesn't fly.
-- A theorem or an example may be a long sequence of stuff, I should not
-- restrict the number of paragraphs. Forbidding anonymous blocks (but at
-- the start after a header possibly ?). Is the best way to go.

-- TODO: think of h3 vs bold. Pandoc could do some of the work for me
--       if I use h3 correctly right ? But the default LaTeX rendering
--       is going to be different ? Anyway, if I use strong, I'll have 
--       to turn the "blocks" into sections.

-- Uhu, sometimes the split is weird before figures (does not include them ?) 
-- (but now always ? See just above "Standard Defining Functions" in the 
-- hyperfunction document. The figure seems to produce its own paragraph
-- somehow (at the HTML level).

-- Short-term: when a Math equation ends with a ".", the paragraph is over. Yes ?
-- No. That's just the end of a SENTENCE.

-- Shit: I have to recognize that a new paragraph that starts with bold is
-- a closer too. (Say theorem that ends with a list followed by a new theorem).

isCloser :: Block -> Bool
isCloser HorizontalRule = True
isCloser (Header _ _ _) = True
isCloser (Div _ _) = True
isCloser _ = False

endsWithPara :: [Block] -> Bool
endsWithPara [] = False
endsWithPara blocks = case last blocks of
  (Para _) -> True
  _ -> False

isPara :: Block -> Bool
isPara (Para _) = True
isPara _ = False

select :: ([Block], [Block]) -> ([Block], [Block])
select ([], block:extra) = select ([block], extra)
select (blocks, block:extra)
  | isCloser block = (blocks, block:extra)
  | (endsWithPara blocks) && (isPara block) = (blocks, block:extra)
  | otherwise = select (blocks ++ [block], extra)
select (blocks, []) = (blocks, [])

wrap :: [Block] -> Block
wrap blocks = Div ([], ["paragraph"], []) blocks

visit :: ([Block], [Block]) -> ([Block], [Block])
visit (done, []) = (done, [])
visit (done, (Para inlines):blocks) = 
  let
    (selected, extra) = select ([], (Para inlines):blocks)
  in
    visit (done ++ [wrap selected], extra)
visit (done, block:blocks) = visit (done ++ [block], blocks)

process :: Pandoc -> Pandoc
process (Pandoc meta blocks) = 
    let 
        (blocks', []) = visit ([], blocks)
    in 
        Pandoc meta blocks'

main :: IO ()
main = toJSONFilter process

