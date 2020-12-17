module Main where

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics

data Book = Book
  { title :: T.Text
  , author :: T.Text
  , year :: Int
  } deriving (Show, Generic)

book :: Book
book = Book
  { title = "Learn Haskell"
  , author = "Will Kurt"
  , year = 2017 }

instance ToJSON Book where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Book
  
bookJson :: BC.ByteString
bookJson = encode book

main :: IO ()
main = print "Hi"
