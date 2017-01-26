{-
Clue is a command line application that uses a fictional murder to show the result of different logics.
The below example has one main function isGuilty and four logics with the following scenario:
- bodyFound- a body was found
- hasMotive- the suspect has a motive
- hasMurderWeapon- a murder weapon was found
- atSceneOfCrime- the suspect was at the crimescene

The user can change the values within the Bool logic.
To use Clue, load with ghci, call the main function, and follow the prompts.
-}

import Control.Monad (when)

--Binary Logic
class Logic a where
  _and :: a -> a -> a
  _or :: a -> a -> a
  _not :: a -> a

instance Logic Bool where 
  _and = (&&)
  _or = (||)
  _not = not

--Main function 1
bodyFoundBool :: Bool 
bodyFoundBool = True

--Main function 2
hasMotiveBool :: Bool 
hasMotiveBool = True

--Main function 3
hasMurderWeapon :: Logic a => a -> a -> a
hasMurderWeapon weaponIDed onPerson = weaponIDed `_and` onPerson

hasMurderWeaponBool :: Bool
hasMurderWeaponBool = hasMurderWeapon weaponIDedBool onPersonBool

weaponIDedBool :: Bool 
weaponIDedBool = True

onPersonBool :: Bool 
onPersonBool = True

--Main function 4
atSceneOfCrime :: Logic a => a -> a -> a
atSceneOfCrime hasAlibi witness = _not hasAlibi `_or` witness

atTheSceneBool :: Bool
atTheSceneBool = atSceneOfCrime hasAlibiBool witnessBool

hasAlibiBool :: Bool 
hasAlibiBool = True

witnessBool :: Bool 
witnessBool = True

--Top function with four arguments listed above.
isGuilty :: Logic a => a -> a -> a -> a -> a
isGuilty bodyFound hasMotive hasWeapon atScene = 
  bodyFound `_and`
  hasMotive `_and`
  hasWeapon `_and`
  atScene

--Kleen Logic
--True | False | Unknown
data Kleene = T | F | Unknown deriving Show

instance Logic Kleene where 
  _and F _ = F
  _and _ F = F 
  _and Unknown _ = Unknown 
  _and _ Unknown = Unknown 
  _and _ _ = T
  _or T _ = T
  _or _ T = T
  _or Unknown _ = Unknown
  _or _ Unknown = Unknown
  _or _ _ = F
  _not F = T
  _not T = F
  _not Unknown = Unknown

bodyFoundKleene :: Kleene
bodyFoundKleene = T

hasMotiveKleene :: Kleene
hasMotiveKleene = T

weaponIDedKleene :: Kleene 
weaponIDedKleene = T

onPersonKleene :: Kleene 
onPersonKleene = T

hasMurderWeaponKleene :: Kleene
hasMurderWeaponKleene = hasMurderWeapon weaponIDedKleene onPersonKleene

hasAlibiKleene :: Kleene
hasAlibiKleene = Unknown

witnessKleene :: Kleene 
witnessKleene = F

atTheSceneKleene :: Kleene
atTheSceneKleene = atSceneOfCrime hasAlibiKleene witnessKleene

{-isGuiltyKleene :: Logic a => a -> a -> a -> a -> a
isGuiltyKleene bodyFoundKleene hasMotiveKleene hasMurderWeaponKleene atTheSceneKleene = 
  bodyFoundKleene `_and`
  hasMotiveKleene `_and`
  hasMurderWeaponKleene `_and`
  atTheSceneKleene-}

--Probabilistic Logic
data Prob = Prob Double deriving Show

instance Logic Prob where
  _and (Prob m) (Prob n) = Prob (m * n)
  _or (Prob m) (Prob n) = Prob (m + n)
  _not (Prob m) = Prob (1-m)

bodyFoundProb :: Prob 
bodyFoundProb = Prob 0.99999999999

hasMotiveProb :: Prob 
hasMotiveProb = Prob 0.98

hasAlibiProb :: Prob 
hasAlibiProb = Prob 0.2

witnessProb :: Prob 
witnessProb = Prob 0.0001

atTheSceneProb :: Prob
atTheSceneProb = atSceneOfCrime hasAlibiProb witnessProb

weaponIDedProb :: Prob 
weaponIDedProb = Prob 0.8

onPersonProb :: Prob 
onPersonProb = Prob 0.999

hasMurderWeaponProb :: Prob
hasMurderWeaponProb = hasMurderWeapon weaponIDedProb onPersonProb

{-isGuiltyProb :: Logic a => a -> a -> a -> a -> a
isGuiltyProb bodyFoundProb hasMotiveProb atTheSceneProb hasMurderWeaponProb = 
  bodyFoundProb `_and`
  hasMotiveProb `_and`
  hasMurderWeaponProb `_and`
  atTheSceneProb-}

--Belnap Algorithm
data Belnap = Yes | No | Both | Neither deriving Show

instance Logic Belnap where 
  _and Yes Yes = Yes
  _and No No = Neither
  _and Yes No = Both
  _and No Yes = Both
  _and _ _ = Neither
  _or Yes _ = Neither
  _or _ Yes = Neither
  _or No _ = Neither
  _or _ No = Neither
  _not No = Yes
  _not Yes = Neither
  _not Both = Neither

bodyFoundBelnap = Yes

hasMotiveBelnap :: Belnap 
hasMotiveBelnap =  Yes

hasAlibiBelnap :: Belnap
hasAlibiBelnap = Yes

witnessBelnap :: Belnap
witnessBelnap =  Yes

atTheSceneBelnap :: Belnap
atTheSceneBelnap = atSceneOfCrime hasAlibiBelnap witnessBelnap

weaponIDedBelnap :: Belnap 
weaponIDedBelnap = Yes

onPersonBelnap :: Belnap 
onPersonBelnap =  Yes

hasMurderWeaponBelnap :: Belnap
hasMurderWeaponBelnap = hasMurderWeapon weaponIDedBelnap onPersonBelnap

{-isGuiltyBelnap :: Logic a => a -> a -> a -> a -> a
isGuiltyBelnap bodyFoundBelnap hasMotiveBelnap atTheSceneBelnap hasMurderWeaponBelnap = 
  bodyFoundBelnap `_and`
  hasMotiveBelnap `_and`
  hasMurderWeaponBelnap `_and`
  atTheSceneBelnap-}

--Clue Command Line
main :: IO ()
main = do
  mappedString ["This program shows how different types of logic can affect the guilt of a suspect in a fictional murder.",
                                "Below are four examples of logic used to determine a suspect's guilt.",
                                "1. Boolean Logic",
                                "2. Kleene / Three-Valued / Ternary Logic",
                                "3. Probabilistic Logic",
                                "4. Belnap / Four-Valued / Relevance Logic"]
  response <- promptLine "Pick a logic [1-4] from the above list, or anything else to exit: "
  when (response == "1") $ do
    linebreak
    mappedString ["Do you want automatic or manual?",
                            "1. Automatic",
                            "2. Manual"]
    userAnswer <- promptLine "Do you want to see an automatic answer or map your own? Pick 1 for automatic or 2 for manual. "
    when (userAnswer == "1") $ do
      mappedString [guiltString ++ "Boolean Logic? ",
                                  show $ isGuilty bodyFoundBool hasMotiveBool atTheSceneBool hasMurderWeaponBool,
                                  "All four conditions were true. Therefore, the suspect is guilty."]
      linebreak >> main
    when (userAnswer == "2") $ do
      linebreak
      mappedString ["Answer the following questions with True or False."] --Parse error if input is anything except True or False
      bodyfound <- promptLine "A body was found: "
      motive <- promptLine "There is a motive: "
      scene <- promptLine "The suspect was at the crimescene:  "
      weapon <- promptLine "There is a murder weapon: "
      print $ isGuiltyIO bodyfound motive scene weapon
      linebreak >> main
  when (response == "2") $ do
    mappedString [guiltString ++ "Ternary Logic? ",
                                  show $ isGuilty bodyFoundKleene hasMotiveKleene hasMurderWeaponKleene atTheSceneKleene,
                                  "Ternary Logic factors in the false eyewitness and unknown alibi."]
    uncertainGuilt >> linebreak >> main
  when (response == "3") $ do
    mappedString [guiltString ++ "Probabilistic Logic? ",
                                  show $ isGuilty bodyFoundProb hasMotiveProb atTheSceneProb hasMurderWeaponProb,
                                  "Probabilistic Logic introduces certainty from 0 to 1, or a percentage or likeliness of guilt."]
    uncertainGuilt >> linebreak >> main
  when (response == "4") $ do
    mappedString [guiltString ++ "Relevance Logic? ",
                                  show $ isGuilty bodyFoundBelnap hasMotiveBelnap atTheSceneBelnap hasMurderWeaponBelnap, 
                                  "Relevance Logic introduces a fourth variable of Neither in addition to Yes, No, and Both."]
    uncertainGuilt >> linebreak >> main
      where
        mappedString = mapM putStrLn
        guiltString = "Is the suspect guilty according to "
        uncertainGuilt = putStrLn "Therefore, the suspect's guilt is more uncertain."
        linebreak = putStrLn ("Change the facts of the murder in the codebase to see different outcomes.\n" ++
                                              "-------------------------------------------------------------------------\n\n")
        
promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    getLine

isGuiltyIO :: String -> String -> String -> String -> Bool
isGuiltyIO bodyfound motive scene weapon =  
  read bodyfound &&
  read motive &&
  read scene &&
  read weapon