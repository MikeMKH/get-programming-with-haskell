{-# LANGUAGE OverloadedStrings #-}

module Main where
  import qualified Data.Text.IO as TIO
  import Palindrome (isPalindrome)
  
  main :: IO()
  main = do
    TIO.putStrLn "Enter a word and I'll tell you if it is a palindrome."
    text <- TIO.getLine
    let response = if isPalindrome text
                     then "it is"
                     else "it is not"
    TIO.putStrLn response