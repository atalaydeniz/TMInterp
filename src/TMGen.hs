module TMGen where 

import ParseTM
import TMCollection
import Data.List

addRule :: Rule -> TM -> TM
addRule r (a, rx, s, t, i) = (a, r:rx, s, t, i) 

generateR_ :: Alphabet -> TM 
generateR_ [] = ([], [
    (ERule (EIn '$' "s0") (EOut 'R' "h")),
    (ERule (EIn '#' "s0") (EOut 'R' "h"))
    ], "s0", "$", 0 :: Int)
generateR_ ax = (ax, [(ERule (EIn a "s0") (EOut 'R' "h")) | a <- ax ++ ['$', '#']], "s0", "$", 0 :: Int)

generateL_ :: Alphabet -> TM
generateL_ [] = ([], [
    (ERule (EIn '$' "s0") (EOut 'L' "h")),
    (ERule (EIn '#' "s0") (EOut 'L' "h"))
    ], "s0", "$", 0 :: Int)
generateL_ ax = (ax, [(ERule (EIn a "s0") (EOut 'L' "h")) | a <- ax ++ ['$', '#']], "s0", "$", 0 :: Int)

generateW_ :: Alphabet -> Char -> TM 
generateW_ [] c = ([], [
    (ERule (EIn '$' "s0") (EOut c "h")),
    (ERule (EIn '#' "s0") (EOut c "h"))
    ], "s0", "$", 0 :: Int)
generateW_ ax c = (ax, [(ERule (EIn a "s0") (EOut c "h")) | a <- ax ++ ['$', '#']], "s0", "$", 0 :: Int)

generateRUntil_ :: Alphabet -> Char -> TM
generateRUntil_ ax c = (ax, [(ERule (EIn c "s0") (EOut c "h"))] ++ [(ERule (EIn a "s0") (EOut 'R' "s0")) | a <- (delete c (ax ++ ['$', '#']))], "s0", "$", 0 :: Int)

