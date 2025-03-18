module TM where
import Data.List
import ParseTM

exTM :: TM 
exTM = (['a'], [
    (ERule (EIn '$' "s0") (EOut 'R' "s1")),
     (ERule (EIn '#' "s1") (EOut 'a' "s1")),
     (ERule (EIn 'a' "s1") (EOut 'R' "s2")),
     (ERule (EIn '#' "s2") (EOut 'a' "s2")),
     (ERule (EIn 'a' "s2") (EOut 'R' "h"))
     ], "s0", "$", 0 :: Int)

findRule :: TM -> Either String Rule
findRule (a, (x:xs), s, t, i) = case x of 
    (ERule (EIn c0 s0) o) -> if (s == s0 && t!!i == c0) then Right x else findRule (a, xs, s, t, i)
findRule (_, [], _, _, _) = Left "Stuck"

applyRule :: TM -> Out -> Either String TM
applyRule (a, r, s, t, i) (EOut 'L' s1) = if (i == 0) then (Left "Index is negative") else (Right (a, r, s1, t, i - 1))
applyRule (a, r, s, t, i) (EOut 'R' s1) = if (i + 1 == length t) then (Right (a, r, s1, t++['#'], i+1)) else (Right (a, r, s1, t, i+1))
applyRule (a, r, s, t, i) (EOut x s1) = Right (a, r, s, replace t x i, i)

replace :: Tape -> Char -> TapeIndex -> Tape
replace t x i = take i t ++ [x] ++ drop (i+1) t

reduceTape :: Tape -> TapeIndex -> Tape
reduceTape t i = if (length t == i + 1) then t else reduceTapeHelper t i (length t) where
    reduceTapeHelper t i l = if t!!(l-1) == '#' && (l /= i + 1) then reduceTapeHelper (init t) i (l-1) else t

prettyPrint :: TM -> IO ()
prettyPrint (a, _, s, t, i) = putStrLn (intersperse ' ' (intersperse '|' t))

prettyPrintEither :: Either String TM -> IO ()
prettyPrintEither (Left error) = putStrLn error
prettyPrintEither (Right x) = prettyPrint x 
    
eval :: TM -> Either String TM
eval (a, [], _, x, _) = Left "No rules are given."
eval (a, r, s, t, i) =  case findRule (a, r, s, t, i) of
    Right (ERule (EIn c0 s0) (EOut c1 s1)) -> case applyRule (a, r, s, t, i) (EOut c1 s1) of
        Right (a, r', "h", t', i') -> Right (a, r', "h", t', i')
        Right (a, r', s', t', i') -> eval (a, r', s', t', i')
        Left error -> Left error
    Left error -> Left error

        