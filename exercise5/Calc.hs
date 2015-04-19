{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Calc where
import ExprT
import Parser
import qualified StackVM as S

-- exercise 1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x1 x2) = (+) (eval x1) (eval x2)
eval (Mul x1 x2) = (*) (eval x1) (eval x2)

--exercise 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

--exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

--exercise4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = lit $ max a b
  mul (MinMax a) (MinMax b) = lit $ min a b

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = lit (x + y)
  mul (Mod7 x) (Mod7 y) = lit (x * y)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

--exercise5
instance Expr S.Program where
  lit a = [S.PushI a]
  add a b = a ++ b ++ [S.Add]
  mul a b = a ++ b ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul

test = fmap S.stackVM $ compile "(3 * -4) + 5"
