module Main where

import TMCollection
import ParseTM
import TM
import Text.Parsec

s = "run(Alphabet:{a,b} \
        \Input: $abab \
        \Rules:($,s0) -> (R, s1) \
      \(#, s1) -> (R, s2) \
      \(#, s2) -> (a, h) \
      \(a, s1) -> (R, s1) \
      \(b, s1) -> (a, h))"

parseProg = case (parse pProg "" s) of 
  Left err -> print err
  Right xs -> evalProg xs

evalProg :: Prog -> IO ()
evalProg [] = print ()
evalProg (x:xs) = case x of
  (EDef tm) -> print ()
  (ERun tm) -> prettyPrintEither (startEval tm) 


main :: IO ()
main = parseProg
