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

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n =
  if remainder == 0
    then False : intToBits' next
    else True : intToBits' next
  where
    remainder = n `mod` 2
    next = n `div` 2

intToBits :: Int -> Bits
intToBits n = padding ++ reverseBits
  where
    reverseBits = reverse $ intToBits' n
    maxBits = length $ intToBits' maxBound
    missingBits = maxBits - (length reverseBits)
    padding = take missingBits $ cycle [False]

charToBits :: Char -> Bits
charToBits c = intToBits $ fromEnum c

bitsToInt :: Bits -> Int
bitsToInt bs = sum $ map (\x -> 2^(snd x)) trues
  where
    size = length bs
    indices = [size-1, size-2 .. 0]
    trues = filter (\x -> fst x == True) $ zip bs indices

bitsToChar :: Bits -> Char
bitsToChar bs = toEnum (bitsToInt bs)

applyOTP' :: String -> String -> [Bits]
applyOTP' pad text = map (\pair -> (fst pair) `xor` (snd pair)) $ zip padBits textBits
  where
    padBits = map charToBits pad
    textBits = map charToBits text
    
applyOTP :: String -> String -> String
applyOTP pad text = map bitsToChar $ applyOTP' pad text

class Chiper a
  where
    encode :: a -> String -> String
    decode :: a -> String -> String

data Rot = Rot

instance Chiper Rot
  where
    encode Rot text = rotEncoder text
    decode Rot text = rotDecoder text

data OneTimePad = OTP String

instance Chiper OneTimePad
  where
    encode (OTP pad) text = applyOTP pad text
    decode (OTP pad) text = applyOTP pad text
  