stringCalculator :: [String] -> Int
stringCalculator (x : "+" : y : xs) = (+) (read x) (read y)
stringCalculator (x : "*" : y : xs) = (*) (read x) (read y)

-- does not work very well but it is what the answer in the back of the book has
mainCalculator :: IO()
mainCalculator = do
  input <- getContents
  let values = lines input
  print $ stringCalculator values

quotes :: [String]
quotes = ["q1", "q2", "q3"]

lookupQuote :: [String] -> [String]
lookupQuote [] = []
lookupQuote ("n":xs) = []
lookupQuote (x:xs) = quote : (lookupQuote xs)
  where
    quote = quotes !! (read x - 1)

-- errors with <stdin>: hGetChar: illegal operation (handle is semi-closed)
-- https://stackoverflow.com/questions/18160807/haskell-io-hgetcontents-illegal-operation-handle-is-closed
main :: IO()
main = do
  input <- getContents
  mapM_ putStrLn $ lookupQuote $ lines input