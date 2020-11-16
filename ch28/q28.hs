{-# LANGUAGE ScopedTypeVariables #-}

addIO' :: Num a => IO a -> IO a -> IO a
addIO' val1 val2 = do
  x <- val1
  y <- val2
  let result = x + y
  return result

addIO :: Num a => IO a -> IO a -> IO a
addIO x y = (+) <$> x <*> y

main :: IO()
main = do
  putStr "x = "
  x :: Int <- readLn
  putStr "y = "
  y :: Int <- readLn
  let lowest = min x y
  putStrLn $ "lowest = " ++ show lowest