import Data.List ()

compareLastNames :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
compareLastNames name1 name2 =
  if lastNameCompare == EQ
    then firstNameCompare
    else lastNameCompare
  where
    lastName1 = snd name1
    lastName2 = snd name2
    firstName1 = fst name1
    firstName2 = fst name2
    lastNameCompare = compare lastName1 lastName2
    firstNameCompare = compare firstName1 firstName2

sfOffice :: ([Char], [Char]) -> [Char]
sfOffice name =
  if lastName < "L"
    then nameText ++ " - PO Box 1234 San Francisco CA 94111"
    else nameText ++ " - PO Box 1010 San Francisco CA 94109"
  where
    lastName = snd name
    firstName = fst name
    nameText = firstName ++ " " ++ lastName
    
nyOffice :: ([Char], [Char]) -> [Char]
nyOffice name =
  nameText ++ ": PO Box 789 - New York, NY, 10013"
  where
    lastName = snd name
    firstName = fst name
    nameText = firstName ++ " " ++ lastName
    
dcOffice :: ([Char], [Char]) -> [Char]
dcOffice name =
  nameText ++ " PO Box 987 Washington DC 13000"
  where
    lastName = snd name
    firstName = fst name
    nameText = firstName ++ " " ++ lastName ++ " Esq"
    
getLocation :: [Char] -> ([Char], [Char]) -> [Char]
getLocation location =
  case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "dc" -> dcOffice
    _ -> (\name -> (fst name) ++ " " ++ (snd name))

addressLetter :: ([Char], [Char]) -> [Char] -> [Char]
addressLetter name location =
  f name
  where f = getLocation location