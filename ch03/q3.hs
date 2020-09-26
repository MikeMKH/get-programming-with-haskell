simple :: p -> p
simple = (\x -> x)

calcChange :: Integer -> Integer -> Integer
calcChange =
  (\owed given -> 
    (\diff -> 
      if diff > 0
        then diff
        else 0) (given - owed))
        
doublePlusTwo :: Integer -> Integer
doublePlusTwo = (\x -> (\doubleX -> doubleX + 2) (x * 2))

counter :: Num a => a -> a
counter x =
  (\x -> x + 1)
    ((\x -> x + 1)
      ((\x -> x) x))