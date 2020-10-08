data Patient = Patient {
  name :: Name,
  sex :: Sex,
  age :: Int,
  height :: Int,
  weight :: Int,
  bloodType :: BloodType
}

type FirstName = String
type MiddleName = String
type LastName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

data Sex = Male | Female

data ABOType = A | B | AB | O
data RhType = Positive | Negative
data BloodType = BloodType ABOType RhType

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False 

donorFor :: Patient -> Patient -> Bool
donorFor p1 p2 = canDonateTo (bloodType p1) (bloodType p2)

patientSummary :: Patient -> String
patientSummary p =
  "***************************************************\n" ++
  "Patient Name: " ++ (nameSummary $ name p) ++ "\n" ++
  "Sex: " ++ (sexSummary $ sex p) ++ "\n" ++
  "Height: " ++ (show $ height p) ++ "\n" ++
  "Weight: " ++ (show $ weight p) ++ "\n" ++
  "Blood Type: " ++ (bloodTypeSummary $ bloodType p) ++ "\n" ++
  "***************************************************\n"

nameSummary :: Name -> String
nameSummary (Name first last) = last ++ ", " ++ first
nameSummary (NameWithMiddle first middle last) = first ++ " " ++ middle ++ " " ++ last

sexSummary :: Sex -> String
sexSummary Male = "male"
sexSummary Female = "female"

bloodTypeSummary :: BloodType -> String
bloodTypeSummary (BloodType abo rh) = (aboTypeSummary abo) ++ (rhTypeSummary rh)

aboTypeSummary :: ABOType -> String
aboTypeSummary A = "A"
aboTypeSummary B = "B"
aboTypeSummary AB = "AB"
aboTypeSummary O = "O"

rhTypeSummary :: RhType -> String
rhTypeSummary Positive = "+"
rhTypeSummary Negative = "-"