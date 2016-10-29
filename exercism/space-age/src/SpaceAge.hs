module SpaceAge (Planet(..), ageOn) where

data Planet = Earth | Mercury | Venus | Mars | Jupiter | Saturn | Uranus | Neptune deriving (Eq, Show)

ageOn :: Planet -> Integer -> Double
ageOn planet seconds = 
  let solarAge = (fromIntegral seconds) / (fromIntegral 31557600)
  in case planet  of 
    Earth -> solarAge
    Mercury -> solarAge / 0.2408467
    Venus -> solarAge / 0.61519726
    Mars -> solarAge / 1.8808158
    Jupiter -> solarAge / 11.862615
    Saturn -> solarAge / 29.447498
    Uranus -> solarAge / 84.016846
    Neptune -> solarAge / 164.79132