main = print (sumDigits [41, 2, 33, 4])
toDigits :: Integer -> [Integer]
toDigits n
  | n < 0     = []
  | n == 0    = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:xs) = [2*x] ++ [y] ++ (doubleEveryOther xs)
doubleEveryOther (x:xs) = [2*x] ++ xs
doubleEveryOther [] = []

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev x = reverse (doubleEveryOther (reverse x))

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate x
  | (sumDigits (doubleEveryOtherRev (toDigits x))) `mod` 10 == 0 = True
  | otherwise = False

type Move = (Peg, Peg)
type Peg = String
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi discNumber from to temp
  | discNumber == 0 = []
  | otherwise       = hanoi (discNumber-1) from temp to ++
                      [(from, to)] ++
                      hanoi (discNumber-1) temp to from
