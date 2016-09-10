--Binary Logic
class Logic a where
  _and :: a -> a -> a 
  _or :: a -> a -> a 
  _not :: a -> a

--Top function
isGuilty :: Logic a => a -> a -> a -> a -> a
isGuilty bodyFound hasMotive hasWeapon atScene = 
  bodyFound `_and`
  hasMotive `_and`
  hasWeapon `_and`
  atScene

instance Logic Bool where 
  _and = (&&)
  _or = (||)
  _not = not

atSceneOfCrime :: Logic a => a -> a -> a
atSceneOfCrime hasAlibi witness = _not hasAlibi `_or` witness

hasMurderWeapon :: Logic a => a -> a -> a
hasMurderWeapon weaponIDed onPerson = weaponIDed `_and` onPerson

--Main function 1
bodyFoundBool :: Bool 
bodyFoundBool = True

--Main function 2
hasMotiveBool :: Bool 
hasMotiveBool = True

weaponIDedBool :: Bool 
weaponIDedBool = True

onPersonBool :: Bool 
onPersonBool = True

--Main function 3
hasMurderWeaponBool :: Bool
hasMurderWeaponBool = hasMurderWeapon weaponIDedBool onPersonBool

hasAlibiBool :: Bool 
hasAlibiBool = True

witnessBool :: Bool 
witnessBool = False

--Main function 4
atTheSceneBool :: Bool
atTheSceneBool = atSceneOfCrime hasAlibiBool witnessBool

--Kleen Logic
--True | False | Unknown
data Kleene = T | F | U deriving Show

instance Logic Kleene where 
  _and F _ = F
  _and _ F = F 
  _and U _ = U 
  _and _ U = U 
  _and _ _ = T
  _or T _ = T
  _or _ T = T
  _or U _ = U
  _or _ U = U
  _or _ _ = F
  _not F = T
  _not T = F
  _not U = U

bodyFoundKleene :: Kleene 
bodyFoundKleene = T

hasMotiveKleene :: Kleene 
hasMotiveKleene = T

hasAlibiKleene :: Kleene 
hasAlibiKleene = U

witnessKleene :: Kleene 
witnessKleene = F

atTheSceneKleene :: Kleene
atTheSceneKleene = atSceneOfCrime hasAlibiKleene witnessKleene

weaponIDedKleene :: Kleene 
weaponIDedKleene = T

onPersonKleene :: Kleene 
onPersonKleene = T

hasMurderWeaponKleene :: Kleene
hasMurderWeaponKleene = hasMurderWeapon weaponIDedKleene onPersonKleene

isGuiltyKleene :: Logic a => a -> a -> a -> a -> a
isGuiltyKleene bodyFoundKleene hasMotiveKleene hasMurderWeaponKleene atTheSceneKleene = 
  bodyFoundKleene `_and`
  hasMotiveKleene `_and`
  hasMurderWeaponKleene `_and`
  atTheSceneKleene

--Probabilistic Logic
data Prob = Prob Double deriving Show

instance Logic Prob where
  _and (Prob m) (Prob n) = (Prob (m * n))
  _or (Prob m) (Prob n) = (Prob (m + n))
  _not (Prob m) = (Prob (1-m))

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

isGuiltyProb :: Logic a => a -> a -> a -> a -> a
isGuiltyProb bodyFoundProb hasMotiveProb atTheSceneProb hasMurderWeaponProb = 
  bodyFoundProb `_and`
  hasMotiveProb `_and`
  atTheSceneProb `_and`
  hasMurderWeaponProb