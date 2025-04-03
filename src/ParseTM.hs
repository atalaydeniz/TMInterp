module ParseTM where

import Text.Parsec hiding (State)

type Alphabet = [Char]
type State = String 
type Tape = String
type TapeIndex = Int

data In = EIn Char State deriving (Show, Eq)
data Out = EOut Char State deriving (Show, Eq)
data Rule = ERule In Out deriving (Show, Eq)

data Gen = EGenR Alphabet               |
           EGenL Alphabet               |
           EGenW Char Alphabet          | 
           EGenRUntil Char Alphabet     |
           EGenLUntil Char Alphabet     |
           EGenRUntilNot Char Alphabet  |
           EGenLUntilNot Char Alphabet  |
           EGenLShift Alphabet          |
           EGenRShift Alphabet
           deriving (Show, Eq)

data Expr = ETM TM | EVar String | EGen deriving (Show, Eq)

data Stmt = EAssign String Expr | ERun Expr deriving (Show, Eq)
type Prog = [Stmt]

type TM = (Alphabet, [Rule], State, Tape, TapeIndex)

pPlus :: Parsec String () Char
pPlus = do
    x <- choice [char '$', char '#']
    return x

pAlphaNumPlus :: Parsec String () Char
pAlphaNumPlus = alphaNum <|> pPlus

pInput :: Parsec String () Tape
pInput = do
    string "Input:"
    spaces
    s <- many1 pAlphaNumPlus
    return s

pCharAndSpace :: Parsec String () Char
pCharAndSpace = do
    a <- letter
    spaces
    return a

pCommaandSpace :: Parsec String () ()
pCommaandSpace = do
    char ','
    spaces
    return ()

pInsideAlphabet :: Parsec String () Alphabet
pInsideAlphabet = do 
    aList <- sepBy (pCharAndSpace) (pCommaandSpace)
    return aList

pAlphabet :: Parsec String () Alphabet
pAlphabet = do
    string "Alphabet:"
    spaces
    char '{'
    spaces
    aList <- pInsideAlphabet
    spaces
    char '}'
    return (aList)

pRule :: Parsec String () Rule
pRule = do
    char '('
    spaces
    a <- pAlphaNumPlus
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
    c <- pAlphaNumPlus
    spaces 
    char ','
    spaces 
    d <- many1 alphaNum
    spaces
    char ')'
    spaces
    return (ERule (EIn a b) (EOut c d))

pTM :: Parsec String () Expr 
pTM = do
    string "begin"
    spaces
    alp <- pAlphabet
    spaces
    inp <- pInput
    spaces
    string "Rules:"
    spaces
    ruleList <- many1 pRule
    spaces
    string "end"
    return (ETM (alp, ruleList, "x", inp, 0))

pVar :: Parsec String () Expr
pVar = do
    var <- many1 alphaNum
    return (EVar var)

pExpr :: Parsec String () Expr
pExpr = do
    spaces
    x <- choice [pTM, pVar]
    spaces
    return x

pRun :: Parsec String () Stmt
pRun = do
    string "run("
    spaces
    e <- pExpr
    spaces
    char ')'
    return (ERun e)

pAssign :: Parsec String () Stmt
pAssign = do
    string "Var"
    spaces
    var <- many1 alphaNum
    spaces
    char '='
    spaces
    e <- pExpr
    return (EAssign var e)

pStmt :: Parsec String () Stmt
pStmt = do
    spaces
    x <- choice [pAssign, pRun]
    spaces
    return x

pProg :: Parsec String () Prog
pProg = do
    x <- many1 pStmt
    return x


