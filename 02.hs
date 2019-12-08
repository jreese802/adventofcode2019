{-# LANGUAGE FlexibleContexts  #-}

module Main where

import Text.Read  (readMaybe)
import Data.Maybe (mapMaybe)

import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as V
import Data.List.Split (splitOn)

import Control.Monad.ST

-- runOp opCode firstAddr secondAddr destAddr program = runSt $ do
--     firstVal = read


-- readProgram :: String -> V.Vector Int
-- readProgram str = V.fromList $ map read $ splitOn "," str
readProgram :: String -> [Int]
readProgram str = map read $ splitOn "," str

-- run the next step of the program
step :: Int -> [Int] -> [Int]
step index prog =  updateList resIndex res prog
    where op = case prog !! index of 1 -> (+)
                                     2 -> (*)
          int1index = prog!!(index + 1)
          int2index = prog!!(index + 2)
          resIndex = prog!!(index + 3)
          res = (prog!!int1index) `op` (prog!!int2index)

run :: Int -> [Int] -> [Int]
run index prog = case prog!!index of 99 -> prog
                                     _  -> run (index + 4) $ step index prog
    where int1index = prog!!(index + 1)
          int2index = prog!!(index + 2)
          resIndex = prog!!(index + 3)

updateList :: Int -> a -> [a] -> [a]
updateList _ _ [] = []
updateList index newVal xs = top ++ (newVal : (tail bottom))
    where (top, bottom) = splitAt index xs

main = do
    programString <- readFile "./inputs/02.input"
    -- let programStr =  (mapMaybe readMaybe :: [String] -> [Integer])  programString
    let programList = readProgram programString
    -- let programVec = readProgram programString
    -- let programVec = V.fromList programList
    -- let programVecM = V.thaw programVec
    -- putStrLn programVecM 
    putStrLn $ show programList
    let programList1 = updateList 1 12 programList
    let programList2 = updateList 2 2 programList1
    putStrLn $ show programList2
    let final = run 0 programList2
    putStrLn $ show final


