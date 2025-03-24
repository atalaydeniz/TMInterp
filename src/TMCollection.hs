module TMCollection where

import ParseTM

r_ :: TM
r_ = ([], [
    (ERule (EIn '$' "s0") (EOut 'R' "h"))
     ], "s0", "$", 0 :: Int) 

l_ :: TM
l_ = ([], [
    (ERule (EIn '$' "s0") (EOut 'L' "h"))
     ], "s0", "$", 0 :: Int) 

sigma_ :: Char -> TM
sigma_ a = ([a], [
    (ERule (EIn '$' "s0") (EOut a "h"))
    ], "s0", "$", 0 :: Int)

runtil_ :: Char -> TM
runtil_ a = ([a], [
    (ERule (EIn '$' "s0") (EOut 'R' "s1")),
    (ERule (EIn a "s1") (EOut a "h")),
    (ERule (EIn '#' "s1") (EOut 'R' "s1"))
    ], "s0", "$", 0 :: Int)
