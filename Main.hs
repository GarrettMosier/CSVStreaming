-- Author: Garrett Mosier
-- Purpose: Calculate statistics for a CSV file / stream

import Data.List.Split

-- TODO Use quickCheck for tests
-- TODO Use lens library to get into records and data types

--data Stats = Textual Int Int Double Double Double | Numeric Int Int Int Int Double deriving Show


-- TODO Use record instead
-- TODO Dynamically generate record from generator exe
data Stats = Textual { count :: Int
                     , nullCount :: Int
                     , shortCount :: Double
                     , longCount :: Double 
                     , averageLen :: Double } | 
             Numeric { count :: Int 
                     , nullCount :: Int 
                     , min :: Int 
                     , max :: Int 
                     , averageVal :: Double } deriving Show


--exampleHeader = "\"sessionId (text)\",\"page (text)\",\"latency (number)\",\"timeOnPage (number)\""
data Header = Header { sessionId :: String 
                     , page :: String 
                     , latency :: Double 
                     , timeOnPage :: Double } deriving Show

data Value = Int | String

defaultNumeric = (Numeric 0 0 0 0 0)
defaultTextual = (Textual 0 0 0 0 0)

f :: Textual -> String -> Textual
f oldStats@(Textual count nullCount shortCount longCount averageLen) msg =
    (Textual (count + 1) nullCount (shortCount + 1) (longCount + 1) (updateAverage count averageLen (length msg)))


g :: Numeric -> Int -> Numeric 
g oldStats@(Numeric count nullCount min max average) newVal =
    (Numeric (count + 1) nullCount min max (updateAverage count average newVal))

updateColumnStatsUnsafe :: Stats -> Value -> Stats
updateColumnStatsUnsafe oldStats val = case val of 
    String -> f oldStats val
    Int -> g oldStats val 

-- Finds the stats for all columns given the new line
updateColumnStatsSafe :: Stats -> Maybe Value -> Stats
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
updateStats :: [Stats] -> Header -> [Stats]
updateStats oldStats columnValues =
    if length oldStats == length columnValues then
        zipWith updateColumnStatsSafe oldStats columnValues
    else
        oldStats

-- Converts parsed line to Header type
toHeader :: [Maybe String] -> Header
toHeader parsedMessage = (Header "" "" 0 0)

-- Converts the line into the delimited form
parseMessage :: String -> [Maybe String]
parseMessage message = map (\x -> if x == "" then Nothing else Just x) $ splitOn "," message

main = do
    -- One stat for each column
    let testStats = [defaultTextual, defaultNumeric, defaultNumeric]
    -- One message for each column
    let messages = toHeader $ parseMessage "TEST,DSA,2d"
    print $ updateStats testStats messages
