q29_1 :: Maybe [Char]
q29_1 = (++) <$> Just "Mike " <*> Just "Harris"

q29_2 :: IO String
q29_2 = pure "Hello World"

allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap func x = pure func <*> x

example :: Int
example = (*) ((+) 2 4) 6

exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> (pure (+) <*> pure 2 <*> pure 4) <*> pure 6

initialBeer :: [Integer]
initialBeer = [6, 12]

numberOfBeer :: [Integer]
numberOfBeer = (-) 4 <$> initialBeer

numberOfFriends :: [Integer]
numberOfFriends = [2, 3]

totalPeople :: [Integer]
totalPeople = (+) 2 <$> numberOfFriends

averageNumberOfBeerDrank :: [Integer]
averageNumberOfBeerDrank = [3, 4]

numberOfBeerNeeded :: [Integer]
numberOfBeerNeeded = (*) <$> totalPeople <*> averageNumberOfBeerDrank

beerToBuy :: [Integer]
beerToBuy = (-) <$> numberOfBeerNeeded <*> numberOfBeer