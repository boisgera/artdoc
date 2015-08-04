#!/usr/bin/env runhaskell

import Text.Pandoc
import Text.Pandoc.JSON
import Data.Char
import Data.List
import Data.List.Split
import Data.Tuple
import Debug.Trace

-- Consistenly wraps sequence of blocks that start with a paragraph with 
-- bold text into a div. The content of the bold text should be something
-- like "Section-Type -- Title" (still in flux, we also may have keywords
-- and/or subtitles, either in parentheses or separated by commas).
-- Possibly trailing . or : ... also, the type could be put in [] to be
-- sure that it's a type (and that probably means that LaTeX should not
-- display it like that).
-- What if there is no "--" separator ? Do we have a type or a title ?
-- I say, let's have a restricted list of keywords so that we can 
-- decide from the context.

-- TODO: think more about the "true sections" vs "lightweight sections".
--       I could do "real" sections for all "chunks" instead of the
--       lightweight bold convention. Think of the pro/cons (see below).

-- TODO: it would be nice generate ids to get the ability to refer to 
--      these sections from markdown. But it seems to work only if we do
--       have sections ... otherwise pandoc does not interpret the
--       "[]" as links ... So actually we would need to turn all this
--       into sections ... but this is still a mess because the "[]"
--       have already been parsed (and not as links), so we would need
--       to make a round-trip to text to get them right. AAAAAARGH !
--       Still, turning these "chunks" into proper sections (say h6)
--       make sense, probably more than div. Well actually, as there is
--       no "Section" type, that will be a Div with a section class, that
--       can be turned into a proper section at (js) runtime.

-- For now, let's not interpret the content of the bold prefix, just segment
-- the stuff into divs.


-- TODO: start on a paragraph with bold stuff at the start, stop on
--       header, new such paragraph, what else ? Fig ? or not ? Should
--       a figure be its own section (eeerrr, dunno, say no ATM, but 
--       that's the lazy choice)

-- Add first stuff after a title if it's a paragraph ? Shit, would need a 
-- "before the block" test :(. Still, that's probably required to "phase out"
-- some text that we don't want in slides ... Dunno ... Can we do it at the
-- css level only ? (as paragraph out of a section or in a section that is 
-- not classified ?). Forbid anomymous sequs of paragraphs ? Nah, this is
-- not pragmatic.
-- Is "starting" just after an end good enough ? Nah, we would put "true"
-- sections into section, that is no good.


section_types = ["proof", 
                 "theorem", 
                 "lemma",
                 "definition", 
                 "notation",
                 "preamble",
                 "context",
                 "remark", 
                 "comment", 
                 "example", 
                 "unknown"] -- use it or not ? Try not to ...

-- TODO: Split a sequence of inlines on "–" (en-dash) or "—" (em-dash).
--       Actually, IT SHOULD BE A EM-DASH.



start :: Block -> Bool
start (Para _) = True
--start (Para []) = False
--start (Para ((Strong (_)):_)) = True
start _ = False -- TODO: think more about this (figures, formulas, etc)

-- there are symbols that END, some that force a NEW, ... and the others.

past_the_end :: Block -> Bool
past_the_end (Header _ _ _) = True
past_the_end HorizontalRule = True
past_the_end (Para ((Strong (_)):_)) = True
past_the_end _ = False

collect_until :: ([Block], [Block]) -> (Block -> Bool) -> ([Block], [Block])
collect_until (blocks, []) _ = (blocks, [])
collect_until (blocks, block:extra) pte = case pte block of
  False -> collect_until (blocks ++ [block], extra) pte
  True -> (blocks, block:extra) 

kosher :: Char -> Bool
kosher '-' = True
kosher '_' = True
kosher char = isAlphaNum char

-- TODO: extract the title, put it in data-title and/or id ? Use the original
--       string (cleaned up) ? As an id instead ?

get_strong_preamble :: [Block] -> [Inline]
get_strong_preamble ((Para ((Strong (inlines)):_)):_) = inlines
get_strong_preamble _ = [Str (""), Str("–"), Str("")]

add_div_ :: ([Block], [Block]) -> ([Block], [Block])
add_div_ (done, []) = (done, [])
add_div_ (done, block:extra) = case start block of
  False -> add_div_ (done ++ [block], extra)
  True -> let
            (blocks', extra') = collect_until ([block], extra) past_the_end
            inlines = get_strong_preamble blocks'
            -- extra fragile ...
            Str type_ = head $ head (splitOneOf [Str("–"), Str("—")] inlines)
            -- TODO: clean up some ":" or "." at the end ? Or even, keep
            type__ = filter kosher $ map toLower type_
            div = Div ("", ["section", type__], []) blocks'     
          in 
            add_div_ (done ++ [div], extra')

add_div :: [Block] -> [Block]
add_div blocks = fst $ add_div_ ([], blocks)

transform :: Pandoc -> Pandoc
transform (Pandoc meta blocks) = Pandoc meta (add_div blocks)

main :: IO ()
main = toJSONFilter transform

