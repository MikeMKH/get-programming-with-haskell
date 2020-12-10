import Data.Char ( isPunctuation )
import Lib ( preprocess, isPalindrome )
import Test.QuickCheck

prop_reverseInvariant text = isPalindrome text == isPalindrome (reverse text)

prop_punctuationInvariant text = preprocess text == preprocess noPunctuationText
  where
    noPunctuationText = filter (not . isPunctuation) text

main :: IO ()
main = do
  quickCheck prop_reverseInvariant
  quickCheck prop_punctuationInvariant
  
