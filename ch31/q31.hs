echo :: IO()
echo = do
  val <- getLine
  putStrLn val

reverseEcho :: IO()
reverseEcho = do
  val <- getLine
  putStrLn $ reverse val