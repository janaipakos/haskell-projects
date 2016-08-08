-- Drink Coffee Simulator
--Main function is getOz with a drinking variable

--TODO Add Type Signatures

-- Declare cup sizes and functions
cup flOz = \message -> message flOz
aCup = cup 6
coffeeCup = cup 12

-- Main drinking function called below
getOz aCup = aCup (\flOz -> flOz) 

drink aCup ozDrank = if (flOz - ozDrank) >= 0
                                         then cup (flOz - ozDrank)
                                         else cup 0
    where flOz = getOz aCup

-- Drinking variables
afterASip = drink coffeeCup 1
afterTwoSips = drink afterASip 1
afterGulp = drink afterTwoSips 4
afterBigGulp = drink coffeeCup 20
afterManySips = foldl drink coffeeCup [1,1,1,1,1]

-- See if a cup is empty
isEmpty aCup = if flOz == 0
                              then True
                              else False
    where flOz = getOz aCup