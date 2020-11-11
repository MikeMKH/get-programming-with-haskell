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

type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString

leaderLength :: Int
leaderLength = 24

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader record = B.take leaderLength record

rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt $ B.take 5 leader

nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest stream = B.splitAt recordLength stream
  where
    recordLength = getRecordLength stream

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords stream =
  if stream == B.empty
    then []
    else next : allRecords rest
  where
    (next, rest) = nextAndRest stream

type MarcDirectoryRaw = B.ByteString

getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt $ B.take 5 $ B.drop 12 leader

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLength afterLeader
  where
    directoryLength = getDirectoryLength record
    afterLeader = B.drop leaderLength record

type MarcDirectoryEntryRaw = B.ByteString

directoryEntryLength :: Int
directoryEntryLength = 12

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory =
  if directory == B.empty
    then []
    else next : splitDirectory rest
  where
    (next, rest) = B.splitAt directoryEntryLength directory

data FieldMetadata = FieldMetadata {
   tag :: T.Text
  ,fieldLength :: Int
  ,fieldStart :: Int
} deriving Show

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata theTag theLength theStart
  where
    (rawTag, rest) = B.splitAt 3 entry
    theTag = E.decodeUtf8 rawTag
    (rawLength, rawStart) = B.splitAt 4 rest
    theLength = rawToInt rawLength
    theStart = rawToInt rawStart

getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata entries = map makeFieldMetadata entries

type FieldText = T.Text

getFieldText :: MarcRecordRaw -> FieldMetadata -> FieldText
getFieldText record metadata = E.decodeUtf8 byteStringValue
  where
    recordLength = getRecordLength record
    baseAddress = getBaseAddress record
    baseRecord = B.drop baseAddress record
    baseAtEntry = B.drop (fieldStart metadata) baseRecord
    byteStringValue = B.take (fieldLength metadata) baseAtEntry

-- downloaded from https://archive.org/download/marc_oregon_summit_records/catalog_files/
-- renamed to sample.mrc and placed in same directory as source file
marcFileName :: String
marcFileName = "sample.mrc"

main :: IO()
main = do
  raw <- B.readFile marcFileName
  let records = allRecords raw
  print $ length records