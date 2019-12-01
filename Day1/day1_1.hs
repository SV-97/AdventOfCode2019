import           System.Environment

type Mass = Integer

type Fuel = Integer

fuelForModule :: Mass -> Fuel
fuelForModule mass = (mass `div` 3) - 2

main = do
  input <- readFile "input1_1.txt"
  let vals = map read . lines $ input
  print . sum . map fuelForModule $ vals
