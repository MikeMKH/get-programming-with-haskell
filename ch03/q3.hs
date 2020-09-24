simple = (\x -> x)

calcChange =
  (\owed given -> 
    (\diff -> 
      if diff > 0
        then diff
        else 0) (given - owed))
        
doublePlusTwo = (\x -> (\doubleX -> doubleX + 2) (x * 2))

counter x =
  (\x -> x + 1)
    ((\x -> x + 1)
      ((\x -> x) x))