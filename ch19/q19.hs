import Data.Maybe ( isNothing, isJust )

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

sample :: [Maybe Organ]
sample = [(Just Brain), Nothing, (Just Heart), (Just Spleen), Nothing, (Just Spleen)]

emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers contents = length . filter isNothing $ contents

fullDrawers :: [Maybe Organ] -> Int
fullDrawers contents = length . filter isJust $ contents

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f Nothing = Nothing
maybeMap f (Just x) = Just (f x)