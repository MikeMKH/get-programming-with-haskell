reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

fib :: (Eq t1, Num t1, Num t2) => t1 -> t2
fib n = fib' 1 1 n
  where
    fib' _ _ 0 = 0
    fib' _ _ 1 = 1
    fib' _ _ 2 = 1
    fib' a b 3 = a + b
    fib' a b n = fib' b (a + b) (n - 1)
    
collatz :: (Num p, Integral a) => a -> p
collatz 1 = 1
collatz n =
  if even n
    then (+) 1 $ collatz $ n `div` 2
    else (+) 1 $ collatz $ (n * 3) + 1

ackermann :: (Num a, Num t, Eq a, Eq t) => a -> t -> t
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m -1) $ ackermann m (n - 1)