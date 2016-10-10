module Turtle where

--Make and fight turtles against each other
--Main function is printTurtle round_
--Lamba function to make new turtle
turtle :: (String, Integer, Integer) -> ((String, Integer, Integer) -> t) -> t
turtle (name,attack,hp) = \x -> x (name,attack,hp)

killerTurtle = turtle ("Bad Turtle", 25, 200)
ninjaTurtle = turtle ("Leo", 30, 150)
filburtTurtle = turtle ("Filburt", 10, 100)

{- Getters and Setter -}
--Wildcards for getters and setters below
name (n,_,_) = n
attack (_,n,_) = n
hp (_,_,n) = n

--Getters for three stats
getName aTurtle = aTurtle name
getAttack aTurtle = aTurtle attack
getHP aTurtle  = aTurtle hp

--Setters for three stats
setName aTurtle newName = aTurtle (\(n,a,h) -> turtle (newName,a,h))
setAttack aTurtle newAttack = aTurtle (\(n,a,h) -> turtle (n,newAttack,h))
setHP aTurtle newHP = aTurtle (\(n,a,h) -> turtle (n,a,newHP))

{- Calculate matchups -}
--Subtract damage from hp
damage aTurtle attackDamage = aTurtle (\(n,a,h) -> turtle (n,a,(h - attackDamage)))

--Compare two turtles. Damage takes the turtle and their attack,
-- and compares it to the defender and their attack
fight aTurtle defender = damage defender attack
    where attack = if (getHP aTurtle) > 0
                               then getAttack aTurtle 
                               else 0

{- Two ways to customize turtles: set attributes to existing or create new -}
--Modify the killerTurtle with new name, attack, and HP
nicerTurtle = setName killerTurtle "Kitty" 
gentlerTurtle = setAttack killerTurtle 5 
softerTurtle = setHP killerTurtle 50

--Add defender turtles
gentleTurtle = turtle ("Nice Turtle", 10, 300)
fastTurtle = turtle ("Speedy", 15, 40) 
slowTurtle = turtle ("Slowpoke",20,30)

{- Fighting Rounds -}
--Three rounds of harmless fighting
gentleTurtleRound1 = fight killerTurtle gentleTurtle
killerTurtleRound1 = fight gentleTurtle killerTurtle
gentleTurtleRound2 = fight killerTurtleRound1 gentleTurtleRound1
killerTurtleRound2 = fight gentleTurtleRound1 killerTurtleRound1
gentleTurtleRound3 = fight killerTurtleRound2 gentleTurtleRound2
killerTurtleRound3 = fight gentleTurtleRound2 killerTurtleRound2

--Demonstrates state. Does not matter what order these are called!
slowTurtleRound1 = fight fastTurtle slowTurtle 
fastTurtleRound1 = fight slowTurtleRound1 fastTurtle
slowTurtleRound2 = fight fastTurtleRound1 slowTurtleRound1 
fastTurtleRound2 = fight slowTurtleRound2 fastTurtleRound1
slowTurtleRound3 = fight fastTurtleRound2 slowTurtleRound2
fastTurtleRound3 = fight slowTurtleRound3 fastTurtleRound2

--Display the turtle stats
printTurtle :: (Show a, Show a1) => (((String, a, a1) -> String) -> t) -> t
printTurtle aTurtle = aTurtle (\(n,a,h) -> "Name: " ++ (n) ++ " Attack:" ++ (show a) ++ " HP:" ++ (show h))


--TODO
--Add type constructors to functions
--Add I/O with getline..have the user type in the turtle attributes and then have them fight
main :: IO ()
main = do 
  putStrLn "Let's create a turtle"
  putStrLn "What is their name?"; x <- getLine
  putStrLn "What is their attack?"; y <- getLine
  putStrLn "What is their hp?"; z <- getLine
  putStrLn ("Name: " ++ x ++ " Attack: " ++ y ++ " HP: " ++ z)

