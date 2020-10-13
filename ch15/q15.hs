data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)
data ThreeLetterAlphabet = Alpha | Beta | Kappa deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where
    halfAlphabetSize = alphabetSize `div` 2
    offset = fromEnum c + halfAlphabetSize
    rotation = offset `mod` alphabetSize
    
rotNDecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNDecoder n c = toEnum rotation
  where
    halfN = n `div` 2
    offset = if even n
      then fromEnum c + halfN
      else 1 + fromEnum c + halfN
    rotation = offset `mod` n
    
rotChar :: Char -> Char
rotChar c = rotN alphabetSize c
  where
    alphabetSize = 1 + fromEnum (maxBound :: Char)

rotEncoder :: String -> String
rotEncoder text = map rotChar text
  where
    alphabetSize = 1 + fromEnum (maxBound :: Char)
    rotChar = rotN alphabetSize

rotDecoder :: String -> String
rotDecoder text = map rotChar text
  where
    alphabetSize = 1 + fromEnum (maxBound :: Char)
    rotChar = rotNDecoder alphabetSize

xorBool :: Bool -> Bool -> Bool
xorBool x y = (x || y) && (not (x && y))

xorPair :: (Bool, Bool) -> Bool
xorPair (x, y) = xorBool x y

xor :: [Bool] -> [Bool] -> [Bool]
xor xs ys = map xorPair $ zip xs ys