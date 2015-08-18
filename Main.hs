-- Author: Garrett Mosier
-- Purpose: Calculate statistics for a CSV file / stream

import Data.List.Split

-- TODO Use quickCheck for tests
-- TODO Use lens library to get into records and data types

-- TODO Use record instead
-- TODO Dynamically generate record from generator exe
data ColumnStat = Textual { count :: Int
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
data Header = Header { sessionId :: Maybe String
                     , page :: Maybe String
                     , latency :: Maybe Double
                     , timeOnPage :: Maybe Double } deriving Show

defaultNumeric = (Numeric 0 0 0 0 0)
defaultTextual = (Textual 0 0 0 0 0)

updateColumnStatsUnsafeDouble :: ColumnStat -> Double -> ColumnStat
updateColumnStatsUnsafeDouble oldStats@(Textual count nullCount shortCount longCount averageLen) val = 
    defaultTextual
updateColumnStatsUnsafeDouble oldStats@(Numeric count nullCount minVal maxVal average) val = 
    (Numeric (count + 1) nullCount minVal maxVal (updateAverage count average val))

updateColumnStatsUnsafeString :: ColumnStat -> String -> ColumnStat
updateColumnStatsUnsafeString oldStats@(Textual count nullCount shortCount longCount averageLen) val =
    (Textual (count + 1) nullCount (shortCount + 1) (longCount + 1) (updateAverage count averageLen (fromIntegral (length val))))
updateColumnStatsUnsafeString oldStats@(Numeric count nullCount minVal maxVal average) val = defaultNumeric


-- Finds the stats for all columns given the new line
updateColumnStatsSafeDouble :: ColumnStat -> Maybe Double -> ColumnStat
updateColumnStatsSafeDouble oldStats@(Textual count nullCount shortCount longCount averageLen) Nothing =
    (Textual count (nullCount + 1) shortCount longCount averageLen)
updateColumnStatsSafeDouble oldStats@(Numeric count nullCount minVal maxVal average) Nothing =
    (Numeric count (nullCount + 1) minVal maxVal average)
updateColumnStatsSafeDouble oldStats (Just val) = updateColumnStatsUnsafeDouble oldStats val

-- TODO Find better way to generalize this information
updateColumnStatsSafeString :: ColumnStat -> Maybe String -> ColumnStat
updateColumnStatsSafeString oldStats@(Textual count nullCount shortCount longCount averageLen) Nothing =
    (Textual count (nullCount + 1) shortCount longCount averageLen)
updateColumnStatsSafeString oldStats@(Numeric count nullCount minVal maxVal average) Nothing =
    (Numeric count (nullCount + 1) minVal maxVal average)
updateColumnStatsSafeString oldStats (Just val) = updateColumnStatsUnsafeString oldStats val



-- Calculates the average value
updateAverage :: Int -> Double -> Double -> Double
updateAverage count oldAverage updateVal = (oldAverage * fromIntegral count + updateVal) / (fromIntegral (count + 1))

-- Finds the new stats for the file with the new line
-- Both input lists must be the same size
updateStats :: [ColumnStat] -> Header -> [ColumnStat]
-- TODO Remove hardcode of header size
updateStats oldStats@(colA:colB:colC:colD:xs) columnValues@(Header a b c d) = [updateColumnStatsSafeString colA a, updateColumnStatsSafeString colB b, updateColumnStatsSafeDouble colC c, updateColumnStatsSafeDouble colD d]
updateStats oldStats _ = oldStats

-- Converts parsed line to Header type
-- TODO Check to see if this deals with Nothings well
toHeader :: [Maybe String] -> Header
toHeader (a:b:c:d:xs) = (Header a b (fmap read c :: Maybe Double) (fmap read d :: Maybe Double))
toHeader _ = (Header (Just "") (Just "") (Just 0) (Just 0))

-- Converts the line into the delimited form
parseMessage :: String -> [Maybe String]
parseMessage message = map (\x -> if x == "" then Nothing else Just x) $ splitOn "," message

main = do
    -- One stat for each column
    let testStats = [defaultTextual, defaultNumeric, defaultNumeric]
    -- One message for each column
    let message = toHeader $ parseMessage "TEST,DSA,1.0,2.1"
    print $ updateStats testStats message
