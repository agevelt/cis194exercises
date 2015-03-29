
import Data.Char
import Data.List
fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

main = print [1,2,3,4,5,6]

fun2 :: Integer -> Integer
fun2 n = sum $ filter even $ takeWhile (/= 1) $ iterate (\x -> if even x
                                                               then (x `div` 2)
                                                               else (3 * x + 1)) n

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

xor :: [Bool] -> Bool
xor = foldr (\x y -> x && not y) False . filter (== True)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> (f x):xs) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base $ reverse xs

asInt_fold :: String -> Int
asInt_fold ('-':xs) = -1 * (asInt_fold (filter isDigit xs))
asInt_fold xs = foldl step 0 xs
  where step a b = a*10 + (digitToInt b)

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

tupleUp xs ys = (xs, ys)

sieveFilter :: [Integer] -> ([Integer], [Integer])
sieveFilter xs = tupleUp xs $ map (\(i,j) -> i+j+(2*i*j)) . filter (\(x, y) -> x <= y) $ cartProd xs xs

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\x -> (x*2)+1) . cartFilter . sieveFilter . enumFromTo 1

cartFilter :: ([Integer], [Integer]) -> [Integer]
cartFilter (xs, ys) = filter (`notElem` ys) xs
