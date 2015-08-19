#!/usr/bin/env runhaskell

import Text.Pandoc
import qualified Text.Pandoc.Builder
import Text.Pandoc.Definition
import Text.Pandoc.JSON
import qualified Text.Pandoc.Shared
import qualified Data.Char
import Data.List
import Data.List.Split (splitOneOf)
import Data.Tuple
import Debug.Trace


-- TODO: some proofs (but not all) are undected and are not sectionned ? 
--       Study the issue ...

-- TODO: deal with "generic" sections, that shall not be displayed.

-- TODO: AFAICT, the tombstone does not mark the end of a proof (because
--       we have a past-the-end only termination scheme). Combine the pte
--       with a "end" scheme ?

-- TODO: see how pandoc flattens nested inlines to get the text and only the
--       text. That can be useful, lead to more robust string extraction.

-- TODO: use data-attributes and CSS to deal with lightweight sections with,
--       says ":" as a separator (we want to display the stuff, but not in
--       the h6 content, so that ids & tocs are fine.)

-- TODO: manage proofs here (now, they are in divs, it's not appropriate/consistant).

-- TODO: deal with separators at the end of lightweight sections. How ?
--       constraints:
--         They display as intended (no sep, ".", ":", this is valid, right ?).
--       Mmm maybe no, strip in Haskell, add . in CSS WHEN REQUIRED (that is,
--       not in TOC mode for example)

-- TODO: support multiple classes (such as in "Theorem & Definition").

-- TODO: replace the "strong header" by some wrapped version of it.
--       (say, span with a class header), then, link this header to
--       the section.

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

-- TODO: commas or --- (em-dashes) without spaces are NOT in separate
--       inlines, so we need to run a "deep split" on inlines to analyze
--       paragraph preambles.

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


--------------------------------------------------------------------------------
-- Lightweight Sections
--------------------------------------------------------------------------------

-- Let's make a proper analysis of the "title" of lighweight/typed sections.
-- The sequence of inlines that represents the title should have the following
-- structure:
--
--   - (optionally) words (alphabetic ? alphanumeric ? hyphen allowed ?
--     do we got HTML(4) and add _, :, . ??? A period at the end would suck ...), 
--     separated by "," or "&": these are the types 
--     (or tags or classes) of the section.
--   - an extra title (optional).
--   - in the middle (if needed, that is if we have type(s) and title), 
--     an en-dash surrounded by spaces or an em-dash (without spaces).
--
--     end separators ? ('.', ':', to get removed & remembered. REALLY ? 
--     '?' can't get removed for example ...)    
--
--   The mess happens when only type or title is present: do we have a 
--   title or types ? I'd say use the following heuristic: if it does 
--   not follow strictly the types pattern, that's a title (say, multiple
--   words separated by spaces without "," or "&", that should rule out
--   95% of the use cases; the use of '?' or '.' can also help.)

--------------------------------------------------------------------------------


enDash :: Char
enDash = '\8211'

emDash :: Char
emDash = '\8212'

dashes :: [Inline]
dashes = [ Str [enDash], Str [emDash] ]

guessSplit :: [Inline] -> ([Inline], [Inline])
guessSplit inlines = case elem Space inlines of
  True -> ([], inlines)  -- this is a title 
  False -> (inlines, []) -- this is a (single) type

split :: [Inline] -> ([Inline], [Inline])
split inlines = 
  let
    parts = splitOneOf dashes inlines
  in
    case length parts of
      3 -> error "too many dashes"
      2 -> (parts !! 0, parts !! 1) 
      1 -> guessSplit (parts !! 0)

toString :: [Inline] -> String
toString (i:is) = 
  let
    str = case i of 
      Str string -> string
      Space -> " "
      Emph inlines -> toString inlines
      Strong inlines -> toString inlines
      _ -> ""
  in
    str ++ (toString is)   
toString [] = ""

strip :: String -> String
strip = f . f
   where f = reverse . dropWhile Data.Char.isSpace

getClasses :: [Inline] -> [String]
getClasses inlines = 
  let 
    types = splitOneOf ",+&" $ toString inlines
    cleanup = map $ (map Data.Char.toLower) . strip
  in
    cleanup types

headerize :: [Block] -> [Block] -- create header for lightweight sections.
headerize ((Para ((Strong inlines):content)):blocks) = 
  let 
    (types, title) = split inlines
    cls = getClasses types
    attr = [] 
  in 
    (headerSeparatorNazi (Header 6 ("", cls, attr) inlines)) : (Para content) : blocks
headerize ((Para content):blocks) = 
  (headerSeparatorNazi (Header 6 ("", [], []) [])) : (Para content) : blocks
headerize x = x

-- TODO: deal with titles ending with "." or ":": strip them and add the sep
--       in a data-sep field. Rk: when there is no sep, add one, UNLESS THE
--       title is actually empty.
--       That may be a little insane ... We can't strip "?" from the title
--       for use in toc for example, but we can't add "." either to it ...

extractSeparator :: [Inline] -> ([Inline], String)
extractSeparator [Str string] = 
  case isSuffixOf "." string of
    True -> ([Str ((reverse . tail. reverse) string)], ".")
    False -> case isSuffixOf ":" string of
      True -> ([Str ((reverse . tail. reverse) string)], ":")
      False -> ([Str string], "") 
extractSeparator (i:is) = 
  let
    (inlines, sep) = extractSeparator is
  in
    (i:inlines, sep)
extractSeparator [] = ([], "")

headerSeparatorNazi :: Block -> Block
headerSeparatorNazi (Header lvl (id, cls, attr) content@(c:cs)) =
  let 
    (content', separator) = extractSeparator content
    attr' = attr ++ [("data-sep", separator)]
  in
    Header lvl (id, cls, attr') content'
headerSeparatorNazi x = x


--------------------------------------------------------------------------------


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
--       Actually, IT SHOULD BE A EM-DASH. (US), but it breaks the grey,
--       so I am gonna stick with the UK convention here ...



start :: Block -> Bool
start (Para _) = True
--start (Para []) = False
--start (Para ((Strong (_)):_)) = True
start _ = False -- TODO: think more about this (figures, formulas, etc)

-- The question is: given the "type"/"title" decomposition, shall I REMOVE
-- the type from the complete title ? Not done now, but think of it
-- (rk: it would probably be ambiguous anyway).


-- Nothing short of horrible ...
get_types :: [Inline] -> [String]
get_types (Str(type_):_) = [type_] 

strip_period :: [Inline] -> [Inline]
strip_period [Str string] = 
  case isSuffixOf "." string of
    True -> [Str ((reverse . tail. reverse) string)]
    False -> [Str string]
strip_period [i] = [i]
strip_period (i:is) = i:strip_period(is)

--kosher :: Char -> Bool
--kosher '-' = True
--kosher '_' = True
--kosher char = isAlphaNum char

--headerize :: [Block] -> [Block] -- create header for lightweight sections.
--headerize ((Para ((Strong inlines):content)):blocks) =
--  let 
--    inlines' = strip_period inlines
--    id = Text.Pandoc.Shared.uniqueIdent inlines' []
--    inlines'' = head $ splitOn dashes inlines'
--    types = get_types inlines'' -- list of Strings
--    cls = map (map toLower) types -- filter kosher (map toLower (head types))
--    attr = [] 
--  in 
--    -- rk: our generated ids may well not be unique ... (say for
--    --     anonymous typed sections such as "Remark.")
--    --     this is an issue for the autolinker, patch afterwards ?
--    --     or generate empty id when no title is detected ?
--    (Header 6 (id, cls, attr) inlines') : (Para content) : blocks
--headerize ((Para content):blocks) = 
--  let strong = Strong [
--                 Str("Generic"), 
--                 Text.Pandoc.Definition.Space, 
--                 Str("–"), 
--                 Text.Pandoc.Definition.Space, 
--                 Str("")
--               ]
--  in
--    headerize $ (Para (strong:content)):blocks
--headerize x = x

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



-- TODO: turn into headers instead of divs (and let the HTML make the sections)

-- TODO: extract the title, put it in data-title and/or id ? Use the original
--       string (cleaned up) ? As an id instead ?


get_strong_preamble :: [Block] -> [Inline]
get_strong_preamble ((Para ((Strong (inlines)):_)):_) = inlines
get_strong_preamble _ = [Str("Generic"), Text.Pandoc.Definition.Space, 
                         Str("–"), Text.Pandoc.Definition.Space, Str("")]

add_div_ :: ([Block], [Block]) -> ([Block], [Block])
add_div_ (done, []) = (done, [])
add_div_ (done, block:extra) = case start block of
  False -> add_div_ (done ++ [block], extra)
  True -> let
            (blocks', extra') = collect_until ([block], extra) past_the_end
            blocks'' = headerize blocks'
--            inlines = get_strong_preamble blocks'
--            -- extra fragile ...
--            Str type_ = head $ head (splitOneOf [Str("–"), Str("—")] inlines)
--            -- TODO: clean up some ":" or "." at the end ? Or even, keep
--            type__ = filter kosher $ map toLower type_
--            div = Div ("", ["section", type__], []) blocks'     
          in 
            add_div_ (done ++ blocks'', extra')
            --add_div_ (done ++ [div], extra')

add_div :: [Block] -> [Block]
add_div blocks = fst $ add_div_ ([], blocks)

transform :: Pandoc -> Pandoc
transform (Pandoc meta blocks) = Pandoc meta (add_div blocks)

main :: IO ()
main = toJSONFilter transform

