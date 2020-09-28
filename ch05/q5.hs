ifEven :: Integral p => (p -> p) -> p -> p
ifEven f x =
  if even x
    then f x
    else x

double :: Num a => a -> a
double = (*) 2

square :: Num a => a -> a
square x = x^2

inc :: Num a => a -> a
inc = (+) 1

ifEvenDouble :: Integer -> Integer
ifEvenDouble = ifEven double

ifEvenSquare :: Integer -> Integer
ifEvenSquare = ifEven square

ifEvenInc :: Integer -> Integer
ifEvenInc = ifEven inc

binaryPartialApplication :: (f -> t1 -> t2) -> f -> t1 -> t2
binaryPartialApplication = \f -> \x -> \y -> f x y