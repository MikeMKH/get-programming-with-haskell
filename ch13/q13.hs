-- *Main> minBound :: Word
-- 0
-- *Main> minBound :: Int
-- -9223372036854775808
-- *Main> maxBound :: Word
-- 18446744073709551615
-- *Main> maxBound :: Int
-- 9223372036854775807

-- *Main> succ $ maxBound :: Int
-- *** Exception: Prelude.Enum.succ{Int}: tried to take `succ' of maxBound
-- *Main> (\x -> x + 1) $ maxBound :: Int
-- -9223372036854775808

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n =
  if maxBound == n
    then minBound
    else succ n
