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
isPrime n | n < 2 = Nothing
          | n > maxPrime = Nothing
          | otherwise = Just $ n `elem` primes

unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors _ [] = []
unsafePrimeFactors n (candidate:xs) =
    if n `mod` candidate == 0
      then candidate : unsafePrimeFactors (n `div` candidate) (candidate:xs)
      else unsafePrimeFactors n xs

primeFactors :: Int -> Maybe [Int]
primeFactors n | n < 2 = Nothing
               | n >= maxPrime = Nothing
               | otherwise = Just $ unsafePrimeFactors n primesLessThanN
  where
    primesLessThanN = filter (<= n) primes
                     