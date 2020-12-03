monthEnds :: [Int]
monthEnds = [31,28,31,30,31,30,31,31,30,31,30,31]

days :: [Int] -> [Int]
days ends = [d | n <- ends, d <- [1 .. n]]

-- daysDo :: [Int] -> [Int]
-- daysDo ends = do
--   n <- ends
--   d <- [1 .. n]
--   return d

daysDo :: [Int] -> [Int]
daysDo ends = do
  n <- ends
  [1 .. n]


-- daysLambda :: [Int] -> [Int]
-- daysLambda ends =
--   ends >>=
--     (\n -> [1 .. n] >>= (\d -> return d))

daysLambda :: [Int] -> [Int]
daysLambda ends =
  ends >>=
    (\n -> [1 .. n])

validDays :: [Int] -> ([Int] -> [Int]) -> ([Int] -> [Int]) -> Bool
validDays ends f g = f ends == g ends