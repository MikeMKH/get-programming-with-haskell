import Control.Monad ( guard )
import Control.Applicative ( Alternative )

data Name = Name
  { firstName :: String
  , lastName :: String }

instance Show Name where
  show (Name first last) = mconcat [ first, " ", last ]

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

_select :: Monad m => (a -> b) -> m a -> m b
_select accessor xs = do
  x <- xs
  return $ accessor x

_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where test xs = do
  x <- xs
  guard $ test x
  return x

startsWith :: Char -> String -> Bool
startsWith letter s = letter == head s

_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a, b)
_join xs ys accessorX accessorY = do
  x <- xs
  y <- ys
  let pairs = (x, y)
  guard $ accessorX (fst pairs) == accessorY (snd pairs)
  return pairs

_hinq :: (t1 -> t2) -> t3 -> (t3 -> t1) -> t2
_hinq selectQuery joinQuery whereQuery =
  (\joinData ->
    (\whereResults -> selectQuery whereResults) (whereQuery joinData)) joinQuery

data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a)
                | HINQ_ (m a -> m b) (m a)

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ s j w) = _hinq s j w
runHINQ (HINQ_ s j) = _hinq s j (_where (\_ -> True))


queryTeacherName :: HINQ [] Teacher Name
queryTeacherName = HINQ_
  (_select teacherName)
  teachers

queryTeacherNameWhoTeachLifeOfCats :: HINQ [] (Teacher, Course) Name
queryTeacherNameWhoTeachLifeOfCats = HINQ
  (_select (teacherName . fst))
  (_join teachers courses teacherId courseTeacher)
  (_where ((== "Life of Cats") . courseTitle . snd))

possibleTeacher :: Maybe Teacher
possibleTeacher = Just $ head teachers

possibleCourse :: Maybe Course
possibleCourse = Just $ head courses

queryMaybeJust :: HINQ Maybe (Teacher, Course) Name
queryMaybeJust = HINQ_
  (_select (teacherName . fst))
  (_join possibleTeacher possibleCourse teacherId courseTeacher)

missingCourse :: Maybe Course
missingCourse = Nothing

queryMaybeNothing :: HINQ Maybe (Teacher, Course) Name
queryMaybeNothing = HINQ
  (_select (teacherName . fst))
  (_join possibleTeacher missingCourse teacherId courseTeacher)
  (_where ((== "The Joy of Flash") . courseTitle . snd))