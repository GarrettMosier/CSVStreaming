-- Author: Garrett Mosier
-- Purpose: Calculate statistics for a CSV file / stream

import Data.List.Split

-- TODO Use record instead
-- TODO Dynamically generate record from generator exe
-- TODO Use quickCheck for tests
-- TODO Use lens library to get into records and data types

data Stats = Textual Int Int Double Double Double | Numeric Int Int Int Int Double deriving Show

defaultNumeric = (Numeric 0 0 0 0 0)
defaultTextual = (Textual 0 0 0 0 0)

updateColumnStatsUnsafe :: Stats -> String -> Stats
updateColumnStatsUnsafe oldStats@(Textual count nullCount shortCount longCount averageLen) msg = 
    (Textual (count + 1) nullCount (shortCount + 1) (longCount + 1) (updateAverage count averageLen (length msg))) 
-- Allow function to take int as well as string so we can calculate averages with it instead
updateColumnStatsUnsafe oldStats@(Numeric count nullCount min max average) msg = 
    (Numeric (count + 1) nullCount min max (updateAverage count average (length msg)))

-- Finds the stats for all columns given the new line 
updateColumnStatsSafe :: Stats -> Maybe String -> Stats 
updateColumnStatsSafe oldStats@(Textual count nullCount shortCount longCount averageLen) Nothing = 
    (Textual count (nullCount + 1) shortCount longCount averageLen) 
updateColumnStatsSafe oldStats@(Numeric count nullCount min max average) Nothing = 
    (Numeric count (nullCount + 1) min max average) 
updateColumnStatsSafe oldStats (Just val) = updateColumnStatsUnsafe oldStats val

-- Calculates the average value
updateAverage :: Int -> Double -> Int -> Double 
updateAverage count oldAverage updateVal = (oldAverage * fromIntegral count + fromIntegral updateVal) / fromIntegral (count + 1)

-- Finds the new stats for the file with the new line
-- Both input lists must be the same size
updateStats :: [Stats] -> [Maybe String] -> [Stats]
updateStats oldStats columnValues = 
    if length oldStats == length columnValues then 
        zipWith updateColumnStatsSafe oldStats columnValues 
    else
        oldStats

-- Converts the line into the delimited form
parseMessage :: String -> [Maybe String]
parseMessage message = map (\x -> if x == "" then Nothing else Just x) $ splitOn "," message

main = do
    -- One stat for each column
    let testStats = [defaultTextual, defaultNumeric, defaultNumeric]
    -- One message for each column
    let messages = parseMessage "TEST,DSA,2d"
    print $ updateStats testStats messages 
    
