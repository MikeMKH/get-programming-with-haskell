tail' :: [a] -> [a]
tail' (_:xs) = xs
tail' [] = []

gcd' :: Integral t => t -> t -> t
gcd' x 0 = x
gcd' a b = gcd' b $ a `mod` b