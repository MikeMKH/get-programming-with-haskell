import Data.Char ( isSpace, toUpper )

elem' :: Eq a => a -> [a] -> Bool
(elem') y xs = 0 < (length $ filter (\x -> x == y) xs)

isPalindrome' :: [Char] -> Bool
isPalindrome' s = cleaned == reverse cleaned
  where
    cleaned = map toUpper $ filter isSpace s

harmonic :: (Fractional a, Enum a) => a -> a
harmonic n = foldr (\x m -> (1 / x) + m) 0 [1 .. n]