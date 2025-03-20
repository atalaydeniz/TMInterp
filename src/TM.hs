module TM where
import Data.List
import ParseTM

---------------------------------------------------------------------------------
-- Alphabet-Tape Checks

sameAlphaLetters :: Alphabet -> Either String Alphabet
sameAlphaLetters a = if (length a == length (nub a)) then Left "ERROR: Alphabet contains same elements." else Right a

inputStartLetter :: Tape -> Either String ()
inputStartLetter (t:ts) = case t of
    '$' -> Right ()
    x -> Left ("ERROR: Tape start character has to be \'$\'. Current character: " ++ show x)

inputNotInAlphabet :: Alphabet -> Tape -> Either String ()
inputNotInAlphabet a (t:ts) = if (elem t a) then inputNotInAlphabet a ts else Left ("ERROR: Character used in tape not contained in alphabet: " ++ show t)
inputNotInAlphabet a [] = Right ()

forbiddenAlphabet :: Alphabet -> Either String ()
forbiddenAlphabet a = if (elem '$' a) || (elem '#' a) then Left ("ERROR: Characters '#' or '$' cannot be included in alphabet") else 
                        if (elem 'L' a) || (elem 'R' a) then Left ("ERROR: Characters 'L' or 'R' cannot be included in alphabet") else
                            Right ()

----------------------------------------------------------------------------------
-- Rule Checks

----------------------------------------------------------------------------------

isStarterRule :: TM -> Bool
isStarterRule (a, r:rs, s, t, i) = case r of
    (ERule (EIn '$' x) (EOut 'R' y)) -> True
    _ -> isStarterRule (a, rs, s, t, i)
isStarterRule (a, [], s, t, i) = False


exTM :: TM 
exTM = (['a'], [
    (ERule (EIn '$' "s0") (EOut 'R' "s1")),
     (ERule (EIn '#' "s1") (EOut 'a' "s1")),
     (ERule (EIn 'a' "s1") (EOut 'R' "s2")),
     (ERule (EIn '#' "s2") (EOut 'a' "s2")),
     (ERule (EIn 'a' "s2") (EOut 'R' "h"))
     ], "s0", "$", 0 :: Int)

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

findRule :: TM -> Either String Rule
findRule (a, (x:xs), s, t, i) = case x of 
    (ERule (EIn c0 s0) o) -> if (s == s0 && t!!i == c0) then Right x else findRule (a, xs, s, t, i)
findRule (_, [], _, _, _) = Left "ERROR: Machine is stuck"

applyRule :: TM -> Out -> Either String TM
applyRule (a, r, s, t, i) (EOut 'L' s1) = if (i == 0) then (Left "ERROR: Index is negative") else (Right (a, r, s1, t, i - 1))
applyRule (a, r, s, t, i) (EOut 'R' s1) = if (i + 1 == length t) then (Right (a, r, s1, t++['#'], i+1)) else (Right (a, r, s1, t, i+1))
applyRule (a, r, s, t, i) (EOut x s1) = if (x /= '#' && (elem x a) == False) then (Left "ERROR: A symbol not in alphabet cannot be written on tape") else
    Right (a, r, s, replace t x i, i)

replace :: Tape -> Char -> TapeIndex -> Tape
replace t x i = take i t ++ [x] ++ drop (i+1) t

reduceTape :: Tape -> TapeIndex -> Tape
reduceTape t i = if (length t == i + 1) then t else reduceTapeHelper t i (length t) where
    reduceTapeHelper t i l = if t!!(l-1) == '#' && (l /= i + 1) then reduceTapeHelper (init t) i (l-1) else t

reduceTapeInput :: Tape -> Tape
reduceTapeInput t = if last t == '#' then reduceTapeInput (init t) else t 

prettyPrint :: TM -> IO ()
prettyPrint (a, _, s, t, i) = putStrLn (intersperse ' ' (intersperse '|' t))

prettyPrintEither :: Either String TM -> IO ()
prettyPrintEither (Left error) = putStrLn error
prettyPrintEither (Right x) = prettyPrint x

startEval :: TM -> Either String TM  
startEval (a, r, s, t, i) = 
    case forbiddenAlphabet a of 
        Left x -> Left x 
        Right () -> case inputNotInAlphabet a t of
            Left x -> Left x
            Right () -> case inputStartLetter t of
                Left x -> Left x
                Right () -> case sameAlphaLetters a of 
                    Left x -> Left x
                    Right a -> eval (a, elimSameRules r, extractStartState r, reduceTapeInput t, i)

eval :: TM -> Either String TM
eval (a, [], _, x, _) = Left "No rules are given."
eval (a, r, s, t, i) =  case findRule (a, r, s, t, i) of
    Right (ERule (EIn c0 s0) (EOut c1 s1)) -> case applyRule (a, r, s, t, i) (EOut c1 s1) of
        Right (a, r', "h", t', i') -> Right (a, r', "h", t', i')
        Right (a, r', s', t', i') -> eval (a, r', s', t', i')
        Left error -> Left error
    Left error -> Left error

        