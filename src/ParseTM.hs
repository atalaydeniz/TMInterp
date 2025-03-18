module ParseTM where

import Text.Parsec hiding (State)

type Alphabet = [Char]
type State = String 
type Tape = String
type TapeIndex = Int

data In = EIn Char State deriving (Show)

data Out = EOut Char State deriving (Show)

data Rule = ERule In Out deriving (Show)

data Stmt = EDef TM | ERun TM deriving (Show)
type Prog = [Stmt]

type TM = (Alphabet, [Rule], State, Tape, TapeIndex)

pInput :: Parsec String () Tape
pInput = do
    string "Input:"
    spaces
    s <- many1 alphaNum
    return s

pAlphabet :: Parsec String () Alphabet
pAlphabet = do
    string "Alphabet:"
    spaces
    char '{'
    spaces
    a <- sepBy letter (char ',')
    spaces
    char '}'
    return a

pRule :: Parsec String () Rule
pRule = do
    char '('
    spaces
    a <- letter
    spaces
    char ','
    spaces
    b <- many1 alphaNum
    spaces
    char ')'
    spaces 
    string "->"
    spaces
    char '('
    spaces
    c <- letter
    spaces 
    char ','
    spaces 
    d <- many1 alphaNum
    spaces
    char ')'
    spaces
    return (ERule (EIn a b) (EOut c d))

pTMDef :: Parsec String () Stmt
pTMDef = do
    alp <- pAlphabet
    spaces
    inp <- pInput
    spaces
    string "Rules:"
    spaces
    ruleList <- many pRule
    return (EDef (alp, ruleList, "x", inp, 0))

pRun :: Parsec String () Stmt
pRun = do
    string "run("
    spaces
    EDef tm <- pTMDef
    spaces
    char ')'
    return (ERun tm)

pStmt :: Parsec String () Stmt
pStmt = do
    spaces
    x <- choice [pTMDef, pRun]
    spaces
    return x

pProg :: Parsec String () Prog
pProg = do
    x <- many pStmt
    return x


