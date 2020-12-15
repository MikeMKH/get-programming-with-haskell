import Data.Char

addStrInts :: String -> String -> Either String Int
addStrInts s1 s2
  | not (all isDigit s1) && not (all isDigit s2) = Left "Neither value could not be parsed"
  | not (all isDigit s1) = Left "First value could not be parsed"
  | not (all isDigit s2) = Left "Second value could not be parsed"
  | otherwise = Right $ value1 + value2
  where
    value1 = read s1
    value2 = read s2

safeSucc :: Int -> Maybe Int
safeSucc x
  | (maxBound :: Int) == x = Nothing
  | otherwise = Just $ succ x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Either String a
safeLast [] = Left "empty list"
safeLast xs = safeLast' 10000 xs
  where
    safeLast' :: Int -> [a] -> Either String a
    safeLast' 0 _ = Left "list exceeds safe bound"
    safeLast' _ [x] = Right x
    safeLast' n (_:xs) = safeLast' (n - 1) xs
    