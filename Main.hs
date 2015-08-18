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
                     , minVal :: Int
                     , maxVal :: Int
                     , averageVal :: Double } deriving Show


--exampleHeader = "\"sessionId (text)\",\"page (text)\",\"latency (number)\",\"timeOnPage (number)\""
data Header = Header { sessionId :: String
                     , page :: String
                     , latency :: Double
                     , timeOnPage :: Double } deriving Show

defaultNumeric = (Numeric 0 0 0 0 0)
defaultTextual = (Textual 0 0 0 0 0)

updateColumnStatsUnsafe :: Stats -> Int -> Stats
updateColumnStatsUnsafe oldStats@(Textual count nullCount shortCount longCount averageLen) val = 
    defaultTextual
updateColumnStatsUnsafe oldStats@(Numeric count nullCount minVal maxVal average) val = 
    (Numeric (count + 1) nullCount minVal maxVal (updateAverage count average val))

updateColumnStatsUnsafe :: Stats -> String -> Stats
updateColumnStatsUnsafe oldStats@(Textual count nullCount shortCount longCount averageLen) val =
    (Textual (count + 1) nullCount (shortCount + 1) (longCount + 1) (updateAverage count averageLen (length val)))
updateColumnStatsUnsafe oldStats@(Numeric count nullCount minVal maxVal average) val = defaultNumeric


-- Finds the stats for all columns given the new line
updateColumnStatsSafe :: Stats -> Maybe Int -> Stats
updateColumnStatsSafe oldStats@(Textual count nullCount shortCount longCount averageLen) Nothing =
    (Textual count (nullCount + 1) shortCount longCount averageLen)
updateColumnStatsSafe oldStats@(Numeric count nullCount minVal maxVal average) Nothing =
    (Numeric count (nullCount + 1) minVal maxVal average)
updateColumnStatsSafe oldStats (Just val) = updateColumnStatsUnsafe oldStats val

-- TODO Find better way to generalize this information
updateColumnStatsSafe :: Stats -> Maybe String -> Stats
updateColumnStatsSafe oldStats@(Textual count nullCount shortCount longCount averageLen) Nothing =
    (Textual count (nullCount + 1) shortCount longCount averageLen)
updateColumnStatsSafe oldStats@(Numeric count nullCount minVal maxVal average) Nothing =
    (Numeric count (nullCount + 1) minVal maxVal average)
updateColumnStatsSafe oldStats (Just val) = updateColumnStatsUnsafe oldStats val



-- Calculates the average value
updateAverage :: Int -> Double -> Int -> Double
updateAverage count oldAverage updateVal = (oldAverage * fromIntegral count + fromIntegral updateVal) / fromIntegral (count + 1)

-- Finds the new stats for the file with the new line
-- Both input lists must be the same size
updateStats :: [Stats] -> Header -> [Stats]
updateStats oldStats columnValues =
    -- TODO Remove hardcode of header size
    if length oldStats == 4 length then
        zipWith updateColumnStatsSafe oldStats columnValues
    else
        oldStats

-- Converts parsed line to Header type
toHeader :: [Maybe String] -> Header
toHeader (a:b:c:d) = (Header a b (read c) (read d))
toHeader _ = (Header "" "" 0 0)

-- Converts the line into the delimited form
parseMessage :: String -> [Maybe String]
parseMessage message = map (\x -> if x == "" then Nothing else Just x) $ splitOn "," message

main = do
    -- One stat for each column
    let testStats = [defaultTextual, defaultNumeric, defaultNumeric]
    -- One message for each column
    let messages = toHeader $ parseMessage "TEST,DSA,2d"
    print $ updateStats testStats messages
