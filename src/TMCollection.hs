module TMCollection where

import TM

r_ :: TM
r_ = ([
    (EStmt (EIn '$' "s0") (EOut 'R' "h"))
     ], "s0", "$", 0 :: Int) 

l_ :: TM
l_ = ([
    (EStmt (EIn '$' "s0") (EOut 'L' "h"))
     ], "s0", "$", 0 :: Int) 

sigma_ :: Char -> TM
sigma_ a = ([
    (EStmt (EIn '$' "s0") (EOut a "h"))
    ], "s0", "$", 0 :: Int)

runtil_ :: Char -> TM
runtil_ a = ([
    (EStmt (EIn '$' "s0") (EOut 'R' "s1")),
    (EStmt (EIn a "s1") (EOut a "h")),
    (EStmt (EIn _ "s1") (EOut 'R' "s1"))
    ], "s0", "$", 0 :: Int)