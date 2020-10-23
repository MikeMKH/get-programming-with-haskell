import qualified Data.Map as Map

helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ "."

inputData :: Map.Map Int String
inputData = Map.fromList [(1, "Mike"), (2, "Kelsey"), (0, "Jack")]

maybeMain :: Maybe String
maybeMain = do
  name <- Map.lookup 2 inputData
  let response = helloPerson name
  return response


fib' :: (Eq t1, Num t1, Num t2) => t2 -> t2 -> t1 -> t2
fib' _ _ 0 = 0
fib' _ _ 1 = 1
fib' _ _ 2 = 1
fib' x y 3 = x + y
fib' x y n = fib' (x + y) x (n - 1)

fib :: Int -> Integer
fib = fib' 1 1

main :: IO ()
main = do
  putStr "nth Fibonacci number: "
  n <- getLine
  let result = fib $ read n
  putStrLn $ show result