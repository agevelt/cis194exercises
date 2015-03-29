{-# LANGUAGE ParallelListComp #-}
import Data.List
skips :: [a] -> [[a]]
skips xs = skipsComp 1 [(x, y) | x <- xs| y <- [1.. length xs]]

skipsComp :: Int -> [(a, Int)] -> [[a]]
skipsComp n xs
  | n == length xs+1 = []
  | otherwise = (map fst $ filter (\(_, y) -> (mod y n) == 0) xs) : skipsComp (n+1) xs

main = print $ skips "abcde"

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs) = (if x < y && y > z then y:(localMaxima (y:z:xs)) else localMaxima (y:z:xs))
localMaxima _ = []

-- TODO: digits not present in input are not displayed. Need way to sort digits into pre-defined buckets.
histogram :: [Int] -> String
histogram xs = (reverse . integersToHistogram . map length . group . sort $ xs)

integersToHistogram :: [Int] -> String
integersToHistogram xs
  | all (<= 0) xs = ""
  | otherwise = concatMap (\x -> if x > 0 then "*" else " ") xs ++ "\n" ++ integersToHistogram (map (subtract 1) xs)

