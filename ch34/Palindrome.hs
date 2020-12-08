module Palindrome (isPalindrome) where
  import qualified Data.Text as T
  import Data.Char (toLower, isSpace, isPunctuation)
  
  stripWhiteSpace :: T.Text -> T.Text
  stripWhiteSpace = T.filter (not . isSpace)
  
  striptPunctuation :: T.Text -> T.Text
  striptPunctuation = T.filter (not . isPunctuation)
  
  toLowerCase :: T.Text -> T.Text
  toLowerCase = T.map toLower
  
  preprocess :: T.Text -> T.Text
  preprocess = stripWhiteSpace . striptPunctuation . toLowerCase
  
  isPalindrome :: T.Text -> Bool
  isPalindrome text = cleanText == T.reverse cleanText
    where
      cleanText = preprocess text