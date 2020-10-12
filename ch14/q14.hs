data Coin = Heads | Tails deriving (Enum, Show)

instance Eq Coin where
  (==) coin1 coin2 = (fromEnum coin1) == (fromEnum coin2)

instance Ord Coin where
  compare coin1 coin2 = compare (fromEnum coin1) (fromEnum coin2)

data FiveSideDice = Side1 | Side2 | Side3 | Side4 | Side5 deriving (Enum, Eq)

instance Show FiveSideDice where
  show Side1 = "1"
  show Side2 = "2"
  show Side3 = "3"
  show Side4 = "4"
  show Side5 = "5"

class (Eq a, Enum a) => Die a where
  roll :: Int -> a

instance Die FiveSideDice where
  roll n = toEnum (n `mod` 5)