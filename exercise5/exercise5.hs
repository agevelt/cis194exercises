import ExprT

-- exercise 1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x1 x2) = (+) (eval x1) (eval x2)
eval (Mul x1 x2) = (*) (eval x1) (eval x2)


-- exercise 2










main = putStr "Hello"
