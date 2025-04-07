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

data Expr = ETM TM | EVar String | EGen Gen deriving (Show, Eq)

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

pEGenR :: Parsec String () Gen 
pEGenR = do 
    string "EGenR("
    spaces
    alp <- pAlphabet
    spaces 
    char ')'
    return (EGenR alp)

pEGenL :: Parsec String () Gen 
pEGenL = do 
    string "EGenL("
    spaces
    alp <- pAlphabet
    spaces 
    char ')'
    return (EGenL alp)

pEGenW :: Parsec String () Gen
pEGenW = do 
    string "EGenW("
    spaces
    ch <- pAlphaNumPlus
    spaces
    char ','
    spaces
    alp <- pAlphabet
    spaces 
    char ')'
    return (EGenW ch alp)

pEGenRUntil :: Parsec String () Gen
pEGenRUntil = do 
    string "EGenRUntil("
    spaces
    ch <- pAlphaNumPlus
    spaces
    char ','
    spaces
    alp <- pAlphabet
    spaces 
    char ')'
    return (EGenRUntil ch alp)

pEGenLUntil :: Parsec String () Gen
pEGenLUntil = do 
    string "EGenLUntil("
    spaces
    ch <- pAlphaNumPlus
    spaces
    char ','
    spaces
    alp <- pAlphabet
    spaces 
    char ')'
    return (EGenLUntil ch alp)

pEGenRUntilNot :: Parsec String () Gen
pEGenRUntilNot = do 
    string "EGenRUntilNot("
    spaces
    ch <- pAlphaNumPlus
    spaces
    char ','
    spaces
    alp <- pAlphabet
    spaces 
    char ')'
    return (EGenRUntilNot ch alp)

pEGenLUntilNot :: Parsec String () Gen
pEGenLUntilNot = do 
    string "EGenLUntilNot("
    spaces
    ch <- pAlphaNumPlus
    spaces
    char ','
    spaces
    alp <- pAlphabet
    spaces 
    char ')'
    return (EGenLUntilNot ch alp)

pEGenRShift :: Parsec String () Gen 
pEGenRShift = do 
    string "EGenRShift("
    spaces
    alp <- pAlphabet
    spaces 
    char ')'
    return (EGenRShift alp)

pEGenLShift :: Parsec String () Gen 
pEGenLShift = do 
    string "EGenL("
    spaces
    alp <- pAlphabet
    spaces 
    char ')'
    return (EGenLShift alp)

pGen :: Parsec String () Expr
pGen = do
    gen <- choice [pEGenR, pEGenL, pEGenRUntil, pEGenLUntil, pEGenRUntilNot, pEGenLUntilNot, pEGenRShift, pEGenLShift]
    return (EGen gen)

pExpr :: Parsec String () Expr
pExpr = do
    spaces
    x <- choice [pTM, pVar, pGen]
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


