import Data.Map as Map

echo :: IO()
echo = do
  val <- getLine
  putStrLn val

reverseEcho :: IO()
reverseEcho = do
  val <- getLine
  putStrLn $ reverse val

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)
data Degree = HS | BS | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
  { candidateId :: Int
  , codeReview :: Grade
  , cultureFit :: Grade
  , education :: Degree } deriving (Show)

viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where
    passedCodeReview = codeReview candidate > B
    passedCultureFit = cultureFit candidate > C
    educationMin = education candidate >= MS
    tests = [ passedCodeReview, passedCultureFit, educationMin ]

readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

readCandidate :: IO Candidate
readCandidate = do
  putStr "id: "
  id <- readInt
  putStr "code review grade: "
  code <- readGrade
  putStr "culture fit grade: "
  fit <- readGrade
  putStr "education: "
  education <- readDegree
  return $ Candidate
    { candidateId = id
    , codeReview = code
    , cultureFit = fit
    , education = education }

assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  let passed = viable candidate
  let statement = if passed
                    then "passed"
                    else "failed"
  return statement

candidate1 :: Candidate
candidate1 = Candidate
  { candidateId = 1
  , codeReview = A
  , cultureFit = B
  , education = MS }

candidate2 :: Candidate
candidate2 = Candidate
  { candidateId = 2
  , codeReview = D
  , cultureFit = C
  , education = PhD }

candidate3 :: Candidate
candidate3 = Candidate
  { candidateId = 3
  , codeReview = A
  , cultureFit = A
  , education = BS }

candidateDB :: Map Int Candidate
candidateDB = Map.fromList
  [ (1, candidate1)
  , (2, candidate2)
  , (3, candidate3) ]

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe id = do
  candidate <- Map.lookup id candidateDB
  let passed = viable candidate
  let statement = if passed
                    then "passed"
                    else "failed"
  return statement

candidates :: [Candidate]
candidates = [candidate1, candidate2, candidate3]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement = if passed
                    then "passed"
                    else "failed"
  return statement

assessCandidate :: Monad m => m Candidate -> m String
assessCandidate candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement = if passed
                    then "passed"
                    else "failed"
  return statement

assessCandidateIO' :: IO String
assessCandidateIO' = assessCandidate readCandidate

assessCandidateMaybe' :: Int -> Maybe String
assessCandidateMaybe' id = assessCandidate $ Map.lookup id candidateDB

assessCandidateList' :: [String]
assessCandidateList' = assessCandidate candidates