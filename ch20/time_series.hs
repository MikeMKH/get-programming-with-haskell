import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

-- data
file1 :: [(Int,Double)]
file1 = [ (1, 200.1), (2,  199.5), (3, 199.4)
        , (4, 198.9), (5,  199.0), (6, 200.2)
        , (9, 200.3), (10, 201.2), (12, 202.9)]

file2 :: [(Int,Double)]
file2 = [(11, 201.6), (12, 201.5), (13, 201.5)
        ,(14, 203.5), (15, 204.9), (16, 207.1)
        ,(18, 210.5), (20, 208.8)]

file3 :: [(Int,Double)]
file3 = [(10, 201.2), (11, 201.6), (12, 201.5)
        ,(13, 201.5), (14, 203.5), (17, 210.5)
        ,(24, 215.1), (25, 218.7)]

file4 :: [(Int,Double)]
file4 = [(26, 219.8), (27, 220.5), (28, 223.8)
        ,(29, 222.8), (30, 223.8), (31, 221.7)
        ,(32, 222.3), (33, 220.8), (34, 219.4)
        ,(35, 220.1), (36, 220.6)]

-- program
data TS a = TS [Int] [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times values = TS timeSeries extendedValues
  where
    timeSeries = [minimum times .. maximum times]
    valueMap = Map.fromList (zip times values)
    extendedValues = map (\v -> Map.lookup v valueMap) timeSeries

fileToTS :: [(Int, a)] -> TS a
fileToTS pairs = createTS times values
  where
    (times, values) = unzip pairs

showTSPair :: Show a => Int -> (Maybe a) -> String
showTSPair time Nothing = mconcat [show time, "|NA\n"]
showTSPair time (Just value) = mconcat [show time, "|", show value, "\n"]

instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows
    where
      rows = zipWith showTSPair times values

ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair m (_, Nothing) = m
insertMaybePair m (key, (Just value)) = Map.insert key value m

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts = ts
combineTS ts (TS [] []) = ts
combineTS (TS t1 v1) (TS t2 v2) = TS times values
  where
    bothTimes = mconcat [t1, t2]
    times = [minimum bothTimes .. maximum bothTimes]
    values' = foldl insertMaybePair Map.empty (zip t1 v1)
    values'' = foldl insertMaybePair values' (zip t2 v2)
    values = map (\v -> Map.lookup v values'') times

instance Semigroup (TS a) where
  (<>) = combineTS

instance Monoid (TS a) where
  mappend = (<>)
  mempty = TS [] []

ts :: TS Double
ts = mconcat [ts1, ts2, ts3, ts4]

mean :: (Real a) => [a] -> Double
mean xs = total / count
  where
    total = (realToFrac . sum) xs
    count = (realToFrac . length) xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS _ values) =
  if all (== Nothing) values
    then Nothing
    else Just avg
  where
    justValues = filter isJust values
    xs = map fromJust justValues
    avg = mean xs

type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare f = g
  where
    g (t1, Nothing) (t2, Nothing) = (t1, Nothing)
    g (_, Nothing) (t, v) = (t, v)
    g (t, v) (_, Nothing) = (t, v)
    g (t1, Just v1) (t2, Just v2) =
      if f v1 v2 == v1
        then (t1, Just v1)
        else (t2, Just v2)

compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS _ (TS [] []) = Nothing
compareTS f (TS times values) =
  if all (== Nothing) values
    then Nothing
    else Just best
  where
    pairs = zip times values
    best = foldl (makeTSCompare f) (0, Nothing) pairs

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _ = Nothing
diffPair _ Nothing = Nothing
diffPair (Just x) (Just y) = Just $ x - y

diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = TS times (Nothing : diffValues)
  where
    shiftedValues = tail values
    diffValues = zipWith diffPair shiftedValues values

meanMaybe :: Real a => [Maybe a] -> Maybe Double
meanMaybe values =
  if any (== Nothing) values
    then Nothing
    else (Just avg)
  where
    avg = mean $ map fromJust values
    
movingAverage :: Real a => [Maybe a] -> Int -> [Maybe Double]
movingAverage [] _ = []
movingAverage values n =
  if length nextValues == n
    then meanMaybe nextValues : movingAverage restValues n
    else []
  where
    nextValues = take n values
    restValues = tail values

movingAverageTS :: Real a => TS a -> Int -> TS Double
movingAverageTS (TS [] []) _ = TS [] []
movingAverageTS (TS times values) n = TS times smoothValues
  where
    ma = movingAverage values n
    padding = replicate (n `div` 2) Nothing
    smoothValues = mconcat [padding, ma, padding]