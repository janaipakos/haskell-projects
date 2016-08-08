--Make and fight robots against each other
--Main function is printRobot round_

--How to make robots. robot and three stats
robot (name,attack,hp) = \message -> message (name,attack,hp)
killerRobot = robot ("Kill3r",25,200)

--Wildcard for getters and setters below
name (n,_,_) = n
attack (_,n,_) = n
hp (_,_,n) = n

--Setters and getters for three stats
getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot  = aRobot hp

setName aRobot newName = aRobot (\(n,a,h) -> robot (newName,a,h))
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot (n,newAttack,h))
setHP aRobot newHP = aRobot (\(n,a,h) -> robot (n,a,newHP))

--Modify the killerRobot with new name, attack, and HP
nicerRobot = setName killerRobot "kitty" 
gentlerRobot = setAttack killerRobot 5 
softerRobot = setHP killerRobot 50

--Display the robot stats
printRobot aRobot = aRobot (\(n,a,h) -> n ++ " attack:" ++ (show a) ++ " hp:" ++ (show h))

--Subtract damage from hp
damage aRobot attackDamage = aRobot (\(n,a,h) -> robot (n,a,(h - attackDamage)))

--Compare two robots. Damage takes the robot and their attack,
-- and compares it to the defender and their attack
fight aRobot defender = damage defender attack
    where attack = if (getHP aRobot) > 0
                               then getAttack aRobot 
                               else 0

--Add defender robots
gentleGiant = robot ("Mr. Friendly", 10, 300)
fastRobot = robot ("speedy", 15, 40) 
slowRobot = robot ("slowpoke",20,30)

--Three rounds of fighting
gentleGiantRound1 = fight killerRobot gentleGiant
killerRobotRound1 = fight gentleGiant killerRobot
gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2

--Three rounds of fighting to demonstrate state. Does not matter what order these are called!
slowRobotRound1 = fight fastRobot slowRobot 
fastRobotRound1 = fight slowRobotRound1 fastRobot
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1 
fastRobotRound2 = fight slowRobotRound2 fastRobotRound1
slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
fastRobotRound3 = fight slowRobotRound3 fastRobotRound2