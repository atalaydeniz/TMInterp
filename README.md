Turing Machine language:

($, s0) -> (R, s1);
(Alphabet, state) -> (Alphabet+Way, state);

Grammar: 
Prog -> Epsilon | Stmts
Stmts -> Stmt | Stmts
Stmt -> In --> Out
In -> (Char, State)
Out -> (Char+Way, State)

Output: 
$ | char | char | ... 