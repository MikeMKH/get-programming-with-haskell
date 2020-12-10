module Lib where

import Data.Char ( isPunctuation )

preprocess :: String -> String
preprocess text = filter (not . isPunctuation) text

isPalindrome :: String -> Bool
isPalindrome text = text == reverse text