repeat' :: a -> [a]
repeat' n = cycle [n]

subseq :: Int -> Int -> [a] -> [a]
subseq start end xs = take (end - start) $ drop start xs

inFirstHalf :: Eq a => a -> [a] -> Bool
inFirstHalf x xs =
  x `elem` firstHalf
  where
    half = (length xs) `div` 2
    firstHalf = take half xs