data Box a = Box a deriving Show

instance Functor Box where
  fmap func (Box a) = Box (func a)

morePresents :: Int -> Box a -> Box [a]
morePresents n box = replicate n <$> box

box :: Box Int
box = Box 8

wrapped :: Box (Box Int)
wrapped = Box <$> box

unwrap :: (Box a) -> a
unwrap (Box val) = val