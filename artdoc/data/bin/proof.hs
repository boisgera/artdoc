#!/usr/bin/env runhaskell

import Text.Pandoc
import Text.Pandoc.JSON
import Data.List
import Debug.Trace

start :: Block -> Bool
start (Para []) = False
start (Para (i:is)) = 
    case i of 
        Strong ((Str string):_) -> isPrefixOf "Proof" string
        _ -> False
start _ = False

end :: Block -> Bool
end (Para []) = False
end (Para inlines) = case last inlines of
    Math InlineMath "\\blacksquare" -> True
    _ -> False
end _ = False

split :: [a] -> (a -> Bool) -> ([a], [a])
split [] p = ([], [])
split (x:xs) p = case p x of
    True -> ([x], xs)
    False -> let 
               (xs', extra) = split xs p
             in 
               (x:xs', extra)

groupProof :: ([Block], [Block]) -> ([Block], [Block])
groupProof (done, []) = (done, [])
groupProof (done, block:blocks) = case start block of
    True -> let
                (blocks', extra) = split (block:blocks) end
                div = Div ("", ["proof"], []) blocks'
            in 
                groupProof (done ++ [div], extra)
    False -> groupProof (done ++ [block], blocks)

docProof :: Pandoc -> Pandoc
docProof (Pandoc meta blocks) = 
    let 
        (blocks', []) = groupProof ([], blocks)
    in 
        Pandoc meta blocks'

main :: IO ()
main = toJSONFilter docProof

