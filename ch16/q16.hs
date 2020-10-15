data StoreItem = BookItem Book
               | RecordItem VinylRecord
               | ToyItem CollectibleToy
               | PamphletItem Pamphlet

data Book = Book {
   bookAuthor :: Creator
  ,bookIsbn :: String
  ,bookTitle :: String
  ,bookYear :: Int
  ,bookPrice :: Double
}

data Creator = AuthorCreator Author
             | ArtistCreator Artist

data Author = Author Name

data Artist = Person Name
            | Band String

data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName
          | TwoInitialsWithLast Char Char LastName
          | FirstWithTwoInitials FirstName Char Char

type FirstName = String
type MiddleName = String
type LastName = String

data VinylRecord = VinylRecord {
   recordArtist :: Creator
  ,recordTitle :: String
  ,recordYear :: Int
  ,recordPrice :: Double
}

data CollectibleToy = CollectibleToy {
   toyName :: String
  ,toyDescription :: String
  ,toyPrice :: Double
}

data Pamphlet = Pamphlet {
   pamphletTitle :: String
  ,pamphletDescription :: String
}

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0.0 

data Shape = Circle Radius
           | Square Height
           | Rectangle Height Width deriving Show

type Radius = Double
type Height = Double
type Width = Double

perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Square h) = 4 * h
perimeter (Rectangle h w) = 2 * (w + h)

area :: Shape -> Double
area (Circle r) = pi * r^2
area (Square h) = h^2
area (Rectangle h w) = w * h