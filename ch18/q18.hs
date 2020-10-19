import qualified Data.Map as Map

data Triple a = Triple a a a deriving Show

mapTriple :: (a -> b) -> Triple a -> Triple b
mapTriple f (Triple x y z) = Triple (f x) (f y) (f z)

data Box a = Box a deriving Show

mapBox :: (a -> b) -> Box a -> Box b
mapBox f (Box x) = Box $ f x

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

values :: [Organ]
values = map snd (Map.toList organCatalog)

organCounts :: [Int]
organCounts = map counts [Heart .. Spleen]
  where
    counts = (\organ -> (length . filter (== organ)) values)

organInventory :: Map.Map Organ Int
organInventory = Map.fromList $ zip [Heart .. Spleen] organCounts