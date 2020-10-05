robot :: (a, b, c) -> ((a, b, c) -> t) -> t
robot (name, attack, hp) = \message -> message (name, attack, hp)

name :: (a, b, c) -> a
name (n,_,_) = n

attack :: (a, b, c) -> b
attack (_,a,_) = a

hp :: (a, b, c) -> c
hp (_,_,h) = h

getName :: (((a, b, c) -> a) -> t) -> t
getName aRobot = aRobot name

getAttack :: (((a, b, c) -> b) -> t) -> t
getAttack aRobot = aRobot attack

getHp :: (((a, b, c) -> c) -> t) -> t
getHp aRobot = aRobot hp

setName :: (((a1, b, c) -> ((a2, b, c) -> t1) -> t1) -> t2) -> a2 -> t2
setName aRobot newName = aRobot (\(n,a,h) -> robot (newName,a,h))

setAttack :: (((a, b1, c) -> ((a, b2, c) -> t1) -> t1) -> t2) -> b2 -> t2
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n,newAttack,h))

setHP :: (((a, b, c1) -> ((a, b, c2) -> t1) -> t1) -> t2) -> c2 -> t2
setHP aRobot newHP = aRobot (\(n,a,h) -> robot (n,a,newHP))


printRobot :: (Show a1, Show a2) => ((([Char], a1, a2) -> [Char]) -> t) -> t
printRobot aRobot =
  aRobot (\(n,a,h) -> n ++ " attack=" ++ (show a) ++ " hp=" ++ (show h))

killerRobot = robot ("Killer",75,200)
weakRobot = setName (setAttack killerRobot 10) "Weak"

damage :: Num c => (((a, b, c) -> ((a, b, c) -> t1) -> t1) -> t2) -> c -> t2
damage aRobot damage = aRobot (\(n,a,h) -> robot (n,a,(h - damage)))

fight :: (Ord a1, Num a1) => (((a2, b1, b1) -> b1) -> a1) -> (((a3, b2, a1) -> ((a3, b2, a1) -> t1) -> t1) -> t2) -> t2
fight attacker defender = damage defender attack
  where
    attack =
      if getHp attacker > 10
        then getAttack attacker
        else 0

-- insane
weakRobotRound1 = fight killerRobot weakRobot
killerRobotRound2 = fight killerRobotRound1 weakRobotRound1
weakRobotRound2 = fight weakRobotRound1 killerRobotRound2
killerRobotRound1 = fight weakRobotRound1 killerRobot