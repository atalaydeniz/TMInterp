A Turing Machine Interpreter.

Language example:

"A = Alphabet: {a, b} 
     Input: $abab
     Rules:
     "($, s0) -> (R, s1)
      (a, s1) -> (R, s2)
      (A, s2) -> (a, h)
      (A-a, s1) -> (R, s1)

B = compose([A, R])

run(B)
"
      