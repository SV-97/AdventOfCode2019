import           System.Environment

clamp :: (Bounded a, Ord a) => a -> a -> a -> a
clamp min max val
  | val <= min = min
  | val >= max = max
  | otherwise = val

type Mass = Int

type Fuel = Int

fuelForMass :: Mass -> Fuel
fuelForMass mass = clamp 0 maxBound $ fuel + fuelForFuel
  where
    fuel = (mass `div` 3) - 2
    fuelForFuel =
      if fuel > 0
        then fuelForMass fuel
        else 0

main = do
  input <- readFile "input1_1.txt"
  let vals = map read . lines $ input
  print . sum . map fuelForMass $ vals
