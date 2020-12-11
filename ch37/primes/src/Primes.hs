module Primes where

maxPrime :: Int
maxPrime = 10000

primes :: [Int]
primes = sieve [2 .. maxPrime]

isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy divisor n | n == 0 = False
                        | otherwise = 0 == n `mod` divisor

sieve :: [Int] -> [Int]
sieve [] = []
sieve (p : xs) = p : sieve candidates
  where
      candidates = filter (not . isDivisibleBy p) xs

isPrime :: Int -> Maybe Bool
isPrime n | n < 0 = Nothing
          | n > maxPrime = Nothing
          | otherwise = Just $ n `elem` primes