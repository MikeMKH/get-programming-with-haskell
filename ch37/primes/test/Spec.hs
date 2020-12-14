import Test.QuickCheck
import Primes
import Data.Maybe

prop_validPrimesOnly value =
  if value < 2 || value >= maxPrime
    then isNothing result
    else isJust result
  where
    result = isPrime value

prop_primesArePrime value =
  if result == Just True
    then length divisors == 0
    else True
  where
    result = isPrime value
    divisors = filter ((== 0) . (value `mod`)) [2 .. (value - 1)]

prop_nonPrimesAreComposite value =
  if result == Just False
    then length divisors > 0
    else True
  where
    result = isPrime value
    divisors = filter ((== 0) . (value `mod`)) [2 .. (value - 1)]

prop_factorsMakeOriginal value =
  if isNothing result
    then True
    else product (fromJust result) == value
  where
    result = primeFactors value

prop_allFactorsArePrime value =
  if isNothing result
    then True
    else all (== Just True) resultArePrimes
  where
    result = primeFactors value
    resultArePrimes = map isPrime (fromJust result)

main :: IO ()
main = do
  quickCheck prop_validPrimesOnly
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_primesArePrime
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_nonPrimesAreComposite
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_factorsMakeOriginal
  quickCheckWith stdArgs { maxSuccess = 1000 } prop_allFactorsArePrime
