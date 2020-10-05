cup :: t1 -> (t1 -> t2) -> t2
cup flOz = \message -> message flOz

getOz :: ((p -> p) -> t) -> t
getOz aCup = aCup (\flOz -> flOz)

drink :: (Ord t1, Num t1) => ((p -> p) -> t1) -> t1 -> (t1 -> t2) -> t2
drink aCup ozDrank =
  if ozDiff >= 0
    then cup ozDiff
    else cup 0
  where
    flOz = getOz aCup
    ozDiff = flOz - ozDrank

isEmpty :: (Eq a, Num a) => ((p -> p) -> a) -> Bool
isEmpty aCup = getOz aCup == 0