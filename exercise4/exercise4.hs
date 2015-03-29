
import Data.Char
import Data.List

-- exercise 1
fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

main = print [1,2,3,4,5,6]

fun2 :: Integer -> Integer
fun2 n = sum $ filter even $ takeWhile (/= 1) $ iterate (\x -> if even x
                                                               then (x `div` 2)
                                                               else (3 * x + 1)) n

-- exercise 2
data Tree a = Leaf
              | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr (\x tree -> insertIntoTree x tree) Leaf

insertIntoTree :: a -> Tree a -> Tree a
insertIntoTree x Leaf = Node 1 Leaf x Leaf
insertIntoTree x (Node n t1 a t2)
  | h1 < h2 =   Node n     intot1 a t2
  | h1 > h2 =   Node n     t1     a intot2
  | otherwise = Node (h+1) t1     a intot2
  where
    h1 = heightTree t1
    h2 = heightTree t2
    intot1 = (insertIntoTree x t1)
    intot2 = (insertIntoTree x t2)
    h = heightTree intot2

heightTree :: Tree a -> Integer
heightTree Leaf = 0
heightTree (Node n _ _ _) = n

-- exercise 3
xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> (f x):xs) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base $ reverse xs

-- random real world haskell exercise
asInt_fold :: String -> Int
asInt_fold ('-':xs) = -1 * (asInt_fold (filter isDigit xs))
asInt_fold xs = foldl step 0 xs
  where step a b = a*10 + (digitToInt b)

-- exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) . filter (`notElem` sieve) $ [1..n]
  where sieve = [x+y+(2*x*y) | x <- [1..n], y <- [1..n], x <= y]
