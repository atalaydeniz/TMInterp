module Main where

import TMCollection
import ParseTM
import TM
import Text.Parsec
import Control.Monad.State.Lazy


s :: String

type Env = [(String, TM)]

s = "run(begin \
        \Alphabet:{a,b} \
        \Input: $abab \
        \Rules:($,s0) -> (R, s1) \
      \(#, s1) -> (R, s2) \
      \(#, s2) -> (a, h) \
      \(a, s1) -> (R, s1) \
      \(b, s1) -> (a, h) \
      \ end)"

s1 = "run(Alphabet:{a, b} \n\
      \Input: $ \n\
      \Rules:($,s0) -> (R, h))"
    
parseProg :: String -> Either ErrorType Prog
parseProg p = case parse pProg "" p of
  Left err   -> Left ParsingError
  Right prog -> Right prog

printParseResult :: Either ErrorType Prog -> IO ()
printParseResult p = case p of 
  Left err -> throwError err
  Right prog -> putStrLn (show prog)

dummy :: StateT Env IO () -> IO ()
dummy d = do
  putStrLn ""

evalStmt :: Prog -> StateT Env IO ()
evalStmt (s:sx) = case s of 
  (EAssign var e) -> do 
    env <- get 
    let e' = evalExpr e env
    put (assignVariable var e' env)
    evalStmt sx
    return ()
  (ERun e) -> do 
    env <- get
    let e' = evalExpr e env
    case startEval e' of 
      (Left error) -> do  
        liftIO $ prettyPrintEither (Left error) 
        return ()
      (Right e'') -> do
        liftIO $ prettyPrintEither (Right e'')
        evalStmt sx 
        return () 

evalExpr :: Expr -> Env -> TM
evalExpr e env = case e of
  (ETM tm) -> tm
 
assignVariable :: String -> TM -> Env -> Env
assignVariable var tm [] = [(var, tm)]
assignVariable var tm ((var', tm') : xs) = if var == var' then ((var, tm) : xs) else (var', tm') : (assignVariable var tm xs)  


-- main :: IO ()
--main = do 
  --x <- readFile "examples/ex1.txt"
  --case parseProg s1 of 
    --(Left err) -> throwError err
    --(Right prog) -> dummy (evalStmt prog)

main :: IO ()
main = do
    x <- readFile "examples/ex2.txt"
    printParseResult (parseProg x) 
    