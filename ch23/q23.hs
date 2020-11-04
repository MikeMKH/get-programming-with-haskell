{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

helloPerson :: T.Text -> T.Text
helloPerson name = mconcat ["Hello ", name, "."]

helloMain :: IO ()
helloMain = do
  putStrLn "What is your name?"
  name <- TIO.getLine
  TIO.putStrLn $ helloPerson name

toInts :: T.Text -> [Int]
toInts = map (read . T.unpack) . T.lines

main :: IO ()
main = do
  input <- TIO.getContents
  let values = toInts input
  TIO.putStrLn $ (T.pack . show . sum) values