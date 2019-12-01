import Text.Read  (readMaybe)
import Data.Maybe (mapMaybe)

fuelForMass :: Integer -> Integer
fuelForMass mass = if x > 0 then x else 0 
    where x = quot mass 3 - 2

totalFuelForMass :: Integer -> Integer
totalFuelForMass mass = sum $ fuelAccum [fuelForMass mass]
    where fuelAccum [] = []
          fuelAccum (0:xs) = xs
          fuelAccum (x:xs) = fuelAccum $ fuelForMass x : x : xs

main :: IO()
main = do
    fileLines <- fmap lines $ readFile "./inputs/01.input"
    -- let fileLines = lines fileText
    let masses = (mapMaybe readMaybe :: [String] -> [Integer])  fileLines
    let part1fuels = sum $ map fuelForMass masses
    let part2fuels = sum $ map totalFuelForMass masses
    putStrLn $ "Part1: " ++  (show part1fuels)
    putStrLn $ "Part2: " ++ (show part2fuels)
    
