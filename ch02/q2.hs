inc :: Int -> Int
inc n = n + 1

double :: Int -> Int
double n = n * 2

square :: Int -> Int
square n = n * n

q23 :: Int -> Int
q23 n =
  if even n
    then n - 2
    else 3 * n - 1