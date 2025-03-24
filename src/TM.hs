module TM where
import Data.List
import ParseTM

data ErrorType = DuplicatedAlphabetError          |
                 TapeStartCharError Char          |
                 InvalidCharinTapeError           |
                 TapeCharNotInAlphabetError Char  |
                 StuckMachineError                |
                 NegativeBoundTapeError           |
                 NoRulesError                     |
                 RuleWriteNonAlphabetCharError Char 

---------------------------------------------------------------------------------
-- Alphabet-Tape Checks

throwError :: ErrorType -> IO ()
throwError e = case e of
    DuplicatedAlphabetError         -> putStrLn "ERROR: Alphabet contains same elements."
    TapeStartCharError c            -> putStrLn ("ERROR: Tape start character has to be \'$\'. Current character: " ++ show c)
    InvalidCharinTapeError          -> putStrLn "ERROR: Characters '#', '$', 'L' or 'R' cannot be included in alphabet"
    TapeCharNotInAlphabetError c    -> putStrLn ("ERROR: Character used in tape not contained in alphabet: " ++ show c)
    StuckMachineError               -> putStrLn "ERROR: Machine is stuck."
    NegativeBoundTapeError          -> putStrLn "ERROR: Index is negative."
    NoRulesError                    -> putStrLn "No rules are given."
    RuleWriteNonAlphabetCharError c -> putStrLn ("ERROR: A symbol not in alphabet cannot be written on tape: " ++ show c)

sameAlphaLetters :: Alphabet -> Either ErrorType ()
sameAlphaLetters a = if ((length a) /= (length (nub a))) then Left DuplicatedAlphabetError else Right ()

inputStartLetter :: Tape -> Either ErrorType ()
inputStartLetter (t:ts) = case t of
    '$' -> Right ()
    x -> Left (TapeStartCharError x)

inputNotInAlphabet :: Alphabet -> Tape -> Either ErrorType ()
inputNotInAlphabet a (t:ts) = case t of
    '$' -> inputNotInAlphabet a ts
    '#' -> inputNotInAlphabet a ts
    x -> if (elem x a) then inputNotInAlphabet a ts else Left (TapeCharNotInAlphabetError x)
inputNotInAlphabet a [] = Right ()

forbiddenAlphabet :: Alphabet -> Either ErrorType ()
forbiddenAlphabet a = if (elem '$' a) || (elem '#' a) || (elem 'L' a) || (elem 'R' a) then Left InvalidCharinTapeError
                            else Right ()

----------------------------------------------------------------------------------
-- Rule Checks

----------------------------------------------------------------------------------

isStarterRule :: TM -> Bool
isStarterRule (a, r:rs, s, t, i) = case r of
    (ERule (EIn '$' x) (EOut 'R' y)) -> True
    _ -> isStarterRule (a, rs, s, t, i)
isStarterRule (a, [], s, t, i) = False

elimSameRules :: [Rule] -> [Rule]
elimSameRules = nub

extractInRule :: Rule -> In 
extractInRule (ERule i o) = i

extractOutRule :: Rule -> Out
extractOutRule (ERule i o) = o

isDTM :: TM -> Bool
isDTM (_, r, _, _, _) = let k = (map extractInRule (elimSameRules r)) in
    (length $ nub $ k) == (length $ k)

extractStartState :: [Rule] -> State
extractStartState (r:rs) = case r of
    (ERule (EIn '$' x) (EOut 'R' y)) -> x
    _ -> extractStartState rs

findRule :: TM -> Either ErrorType Rule
findRule (a, (x:xs), s, t, i) = case x of 
    (ERule (EIn c0 s0) _) -> if (s == s0 && t!!i == c0) then Right x else findRule (a, xs, s, t, i)
findRule (_, [], _, _, _) = Left StuckMachineError

applyRule :: TM -> Out -> Either ErrorType TM
applyRule (a, r, s, t, i) (EOut 'L' s1) = if (i == 0) then Left NegativeBoundTapeError else (Right (a, r, s1, t, i - 1))
applyRule (a, r, s, t, i) (EOut 'R' s1) = if (i + 1 == length t) then (Right (a, r, s1, t++['#'], i+1)) else (Right (a, r, s1, t, i+1))
applyRule (a, r, s, t, i) (EOut x s1) = if (x /= '#' && (elem x a) == False) then Left (RuleWriteNonAlphabetCharError x) else
    Right (a, r, s1, replace t x i, i)

replace :: Tape -> Char -> TapeIndex -> Tape
replace t x i = take i t ++ [x] ++ drop (i+1) t

reduceTape :: Tape -> TapeIndex -> Tape
reduceTape t i = if (length t == i + 1) then t else reduceTapeHelper t i (length t) where
    reduceTapeHelper t i l = if t!!(l-1) == '#' && (l /= i + 1) then reduceTapeHelper (init t) i (l-1) else t

reduceTapeInput :: Tape -> Tape
reduceTapeInput t = if last t == '#' then reduceTapeInput (init t) else t 

prettyPrint :: TM -> IO ()
prettyPrint (a, _, s, t, i) = putStrLn (intersperse ' ' (intersperse '|' t))

prettyPrintEither :: Either ErrorType TM -> IO ()
prettyPrintEither (Left error) = throwError error
prettyPrintEither (Right x) = prettyPrint x

startEval :: TM -> Either ErrorType TM  
startEval (a, r, s, t, i) = 
    case forbiddenAlphabet a of 
        Left x -> Left x 
        Right () -> case inputNotInAlphabet a t of
            Left x -> Left x
            Right () -> case inputStartLetter t of
                Left x -> Left x
                Right () -> case sameAlphaLetters a of 
                    Left x -> Left x
                    Right () -> eval (a, elimSameRules r, extractStartState r, reduceTapeInput t, i)

eval :: TM -> Either ErrorType TM
eval (a, [], _, x, _) = Left NoRulesError
eval (a, r, s, t, i) =  case findRule (a, r, s, t, i) of
    Right (ERule (EIn c0 s0) (EOut c1 s1)) -> case applyRule (a, r, s, t, i) (EOut c1 s1) of
        Right (a, r', "h", t', i') -> Right (a, r', "h", t', i')
        Right (a, r', s', t', i') -> eval (a, r', s', t', i')
        Left error -> Left error
    Left error -> Left error

        