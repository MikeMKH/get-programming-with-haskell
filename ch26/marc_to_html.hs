{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import Data.Maybe

type Author = T.Text
type Title = T.Text

data Book = Book {
   author :: Author
  ,title :: Title
}

type Html = T.Text

bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n", titleTag, authorTag, "</p>\n"]
  where
    titleTag = mconcat ["<strong>", (title book), "</strong>\n"]
    authorTag = mconcat ["<em>", (author book), "</em>\n"]

book1 :: Book
book1 = Book { title = "A Christmas Carol", author = "Charles Dickens" }

book2 :: Book
book2 = Book { title = "The Ickabog", author = "J. K. Rowling" }

book3 :: Book
book3 = Book { title = "Dune", author = "Frank Herbert" }

booksToHtml :: [Book] -> Html
booksToHtml books = mconcat [
   "<html>\n"
  ,"<head><title>books</title>"
  ,"<meta charset='utf-8'/>"
  ,"</head>\n"
  ,"<body>\n"
  ,booksHtml
  ,"\n</body>\n"
  ,"</html>"]
  where
    booksHtml = mconcat . (map bookToHtml) $ books

books :: [Book]
books = [book1, book2, book3]

main :: IO()
main = do
  TIO.writeFile "books.html" $ booksToHtml books