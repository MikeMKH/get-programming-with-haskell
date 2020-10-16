import Data.Semigroup

data Color = Red
           | Yellow
           | Blue
           | Green
           | Purple
           | Orange
           | Brown
           | Clear deriving (Show, Eq)

instance Semigroup Color where
  (<>) Clear any = any
  (<>) any Clear = any
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Red Yellow = Orange
  (<>) Yellow Red = Orange
  (<>) a b | a == b = a
           | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
           | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
           | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
           | otherwise = Brown

data Events = Events [String]
data Probs = Probs [Double]

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine f xs ys = zipWith f newXs newYs
  where
    nToAdd = length ys
    repeatedXs = map (take nToAdd . repeat) xs
    newXs = mconcat repeatedXs
    newYs = cycle ys

combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events (cartCombine combiner e1 e2)
  where
    combiner = (\x y -> mconcat [x, "-", y])

instance Semigroup Events where
  (<>) = combineEvents

instance Monoid Events where
  mappend = (<>)
  mempty = Events []

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2)

instance Semigroup Probs where
  (<>) = combineProbs

instance Monoid Probs where
  mappend = (<>)
  mempty = Probs []