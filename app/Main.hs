module Main where

import TMCollection
import ParseTM
import TM
import Text.Parsec
import Control.Monad.State.Lazy
import Control.Monad.Except 


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
  Left err -> printError err
  Right prog -> putStrLn (show prog)

evalStmt :: Prog -> ExceptT ErrorType (StateT Env IO) ()
evalStmt (s:sx) = case s of 
  (EAssign var e) -> do 
    env <- get 
    case evalExpr e env of
      (Left err) -> do 
        throwError $ err
      (Right tm) -> do
        let e' = tm
        put (assignVariable var e' env)
        evalStmt sx
        return ()
  (ERun e) -> do 
    env <- get
    case evalExpr e env of 
      (Left err) -> do 
        throwError $ err
      (Right tm) -> do
        let e' = tm
        case startEval e' of 
          (Left err) -> do  
            throwError $ err
          (Right e'') -> do
            liftIO $ prettyPrintEither (Right e'')
            evalStmt sx 
            return ()
evalStmt [] = return ()

evalExpr :: Expr -> Env -> Either ErrorType TM
evalExpr e env = case e of
  (ETM tm) -> Right tm
  (EVar var) -> lookupTM var env

lookupTM :: String -> Env -> Either ErrorType TM
lookupTM s [] = Left (VariableNotFoundError s)
lookupTM s ((s', tm') : env) = case s of
  s' -> Right tm'
  _ -> lookupTM s env
 
assignVariable :: String -> TM -> Env -> Env
assignVariable var tm [] = [(var, tm)]
assignVariable var tm ((var', tm') : xs) = if var == var' then ((var, tm) : xs) else (var', tm') : (assignVariable var tm xs)  


-- main :: IO ()
--main = do 
  --x <- readFile "examples/ex1.txt"
  --case parseProg s1 of 
    --(Left err) -> printError err
    --(Right prog) -> dummy (evalStmt prog)

printResult :: Either ErrorType () -> IO ()
printResult (Left err) = printError err
printResult (Right ()) = putStrLn "Success" 

main :: IO (Either ErrorType ())
main = do
    x <- readFile "examples/ex2.txt"
    case (parseProg x) of 
      (Left err) -> undefined
      (Right p) -> evalStateT (runExceptT (evalStmt p)) []
      

    