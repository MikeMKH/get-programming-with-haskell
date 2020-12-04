import Control.Monad

data Name = Name
  { firstName :: String
  , lastName :: String }

instance Show Name where
  show (Name first last) = mconcat [ last, ", ", first ]

data GradeLevel = Freshman
                | Sophomore
                | Junior
                | Senior deriving (Eq, Ord, Enum, Show)

data Student = Student
  { studentId :: Int
  , gradeLevel :: GradeLevel
  , studentName :: Name } deriving Show
  
data Teacher = Teacher
  { teacherId :: Int
  , teacherName :: Name } deriving Show

data Course = Course
  { courseId :: Int
  , courseTitle :: String
  , courseTeacher :: Int } deriving Show

students :: [Student]
students = [(Student 1 Freshman (Name "Tom" "White"))
           ,(Student 2 Sophomore (Name "Bobby" "Ingve"))
           ,(Student 3 Junior (Name "Cindy" "Ann"))
           ,(Student 4 Senior (Name "Jack" "Harris"))
           ,(Student 5 Freshman (Name "Mike" "Harris"))
           ,(Student 6 Junior (Name "Dave" "Smith"))]

teachers :: [Teacher]
teachers = [(Teacher 1 (Name "Kim" "Jones"))
           ,(Teacher 2 (Name "Kelsey" "Hopkins"))]

courses :: [Course]
courses = [(Course 100 "Life of Cats" 1)
          ,(Course 200 "Dog Breeds" 2)]

_select :: (a -> b) -> [a] -> [b]
_select accessor xs = do
  x <- xs
  return $ accessor x

_where :: (a -> Bool) -> [a] -> [a]
_where test xs = do
  x <- xs
  guard $ test x
  return x

startsWith :: Char -> String -> Bool
startsWith letter s = letter == head s

_join :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a, b)]
_join xs ys accessorX accessorY = do
  x <- xs
  y <- ys
  let pairs = (x, y)
  guard $ accessorX (fst pairs) == accessorY (snd pairs)
  return pairs