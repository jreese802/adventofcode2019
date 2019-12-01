import Text.Read  (readMaybe)
import Data.Maybe (mapMaybe)


-- what am I even trying to do here?
-- I want to first calculate the fuel for my payload
-- Then recursively calculate the fuel I need for my fuel
--   Which looks like running calcFuel until it returns 0
-- Thing is... stuff like 'until it returns 0' isn't pure

calcFuel :: Integer -> Integer
calcFuel mass = if x > 0 then x else 0 
    where x = quot mass 3 - 2

fuelAccum :: [Integer] -> [Integer]
fuelAccum [] = []
fuelAccum (0:xs) = xs
fuelAccum (x:xs) = fuelAccum $ calcFuel x : x : xs

calcFuelTotal :: Integer -> Integer
calcFuelTotal mass = sum $ fuelAccum [calcFuel mass]


main :: IO()
main = do
    fileText <- readFile "./inputs/01.input"
    let fileLines = lines fileText
    let masses = (mapMaybe readMaybe :: [String] -> [Integer])  fileLines
    let part1fuels = sum $ map calcFuel masses
    let part2fuels = sum $ map calcFuelTotal masses
    putStrLn $ "Part1: " ++  (show part1fuels)
    putStrLn $ "Part2: " ++ (show part2fuels)
    
