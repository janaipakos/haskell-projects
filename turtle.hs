--Make and fight turtles against each other
--Main function is printTurtle round_

--How to make turtles. turtle and three stats
turtle (name,attack,hp) = \message -> message (name,attack,hp)
killerTurtle = turtle ("Kill3r", 25, 200)

--Wildcard for getters and setters below
name (n,_,_) = n
attack (_,n,_) = n
hp (_,_,n) = n

--Setters and getters for three stats
getName aTurtle = aTurtle name
getAttack aTurtle = aTurtle attack
getHP aTurtle  = aTurtle hp

setName aTurtle newName = aTurtle (\(n,a,h) -> turtle (newName,a,h))
setAttack aTurtle newAttack = aTurtle (\(n,a,h) -> turtle (n,newAttack,h))
setHP aTurtle newHP = aTurtle (\(n,a,h) -> turtle (n,a,newHP))

--Modify the killerTurtle with new name, attack, and HP
nicerTurtle = setName killerTurtle "kitty" 
gentlerTurtle = setAttack killerTurtle 5 
softerTurtle = setHP killerTurtle 50

--Display the turtle stats
printTurtle aTurtle = aTurtle (\(n,a,h) -> n ++ " attack:" ++ (show a) ++ " hp:" ++ (show h))

--Subtract damage from hp
damage aTurtle attackDamage = aTurtle (\(n,a,h) -> turtle (n,a,(h - attackDamage)))

--Compare two turtles. Damage takes the turtle and their attack,
-- and compares it to the defender and their attack
fight aTurtle defender = damage defender attack
    where attack = if (getHP aTurtle) > 0
                               then getAttack aTurtle 
                               else 0

--Add defender turtles
gentleTurtle = turtle ("Mr. Friendly", 10, 300)
fastTurtle = turtle ("speedy", 15, 40) 
slowTurtle = turtle ("slowpoke",20,30)

--Three rounds of fighting
gentleTurtleRound1 = fight killerTurtle gentleTurtle
killerTurtleRound1 = fight gentleTurtle killerTurtle
gentleTurtleRound2 = fight killerTurtleRound1 gentleTurtleRound1
killerTurtleRound2 = fight gentleTurtleRound1 killerTurtleRound1
gentleTurtleRound3 = fight killerTurtleRound2 gentleTurtleRound2
killerTurtleRound3 = fight gentleTurtleRound2 killerTurtleRound2

--Three rounds of fighting to demonstrate state. Does not matter what order these are called!
slowTurtleRound1 = fight fastTurtle slowTurtle 
fastTurtleRound1 = fight slowTurtleRound1 fastTurtle
slowTurtleRound2 = fight fastTurtleRound1 slowTurtleRound1 
fastTurtleRound2 = fight slowTurtleRound2 fastTurtleRound1
slowTurtleRound3 = fight fastTurtleRound2 slowTurtleRound2
fastTurtleRound3 = fight slowTurtleRound3 fastTurtleRound2
