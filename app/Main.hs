module Main where

import ParseTM
import TM
import Text.Parsec
import Control.Monad.State.Lazy
import Control.Monad.Except 

type Env = [(String, TM)]
    
parseProg :: String -> Either ErrorType Prog
parseProg p = case parse pProg "" p of
  Left _   -> Left ParsingError
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
        liftIO $ printError err
        throwError err
      (Right tm) -> do
        put (assignVariable var tm env)
        evalStmt sx
        return ()
  (ERun e) -> do 
    env <- get
    case evalExpr e env of 
      (Left err) -> do 
        liftIO $ printError err
        throwError err
      (Right tm) -> do
        case startEval tm of 
          (Left err) -> do  
            liftIO $ printError err
            throwError err
          (Right e'') -> do
            liftIO $ prettyPrintEither (Right e'')
            evalStmt sx 
            return ()
evalStmt [] = return ()

evalExpr :: Expr -> Env -> Either ErrorType TM
evalExpr e env = case e of
  (ETM tm) -> Right tm
  (EVar var) -> lookupTM var env
  (EGen gen) -> generate gen

lookupTM :: String -> Env -> Either ErrorType TM
lookupTM s [] = Left (VariableNotFoundError s)
lookupTM s ((s', tm') : env) = if (s == s') then Right tm' else lookupTM s env
 
assignVariable :: String -> TM -> Env -> Env
assignVariable var tm [] = [(var, tm)]
assignVariable var tm ((var', tm') : xs) = if (var == var') then ((var, tm) : xs) else (var', tm') : (assignVariable var tm xs) 

generate :: Gen -> Either ErrorType TM 
generate gen = undefined

printResult :: Either ErrorType () -> IO ()
printResult (Left err) = printError err
printResult (Right ()) = putStrLn "Success" 

main :: IO ()
main = do
    x <- readFile "examples/ex3.txt"
    case (parseProg x) of 
      (Left err) -> printError err
      (Right p) -> do 
        progResult <- evalStateT (runExceptT (evalStmt p)) []
        case progResult of
          (Left err) -> printError err
          (Right _) -> putStrLn ""

      

    