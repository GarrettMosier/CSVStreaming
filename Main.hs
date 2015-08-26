-- Author: Garrett Mosier
-- Purpose: Calculate statistics for a CSV file / stream

import Data.List.Split

-- TODO Use quickCheck for tests
-- TODO Use lens library to get into records and data types


-- TODO Dynamically generate record from generator exe
data ColumnStat = Textual { count :: Int
                     , nullCount :: Int
                     , shortCount :: Double
                     , longCount :: Double
                     , averageLen :: Double } |
             Numeric { count :: Int
                     , nullCount :: Int
                     , minVal :: Double
                     , maxVal :: Double
                     , averageVal :: Double } deriving Show


--exampleHeader = "\"sessionId (text)\",\"page (text)\",\"latency (number)\",\"timeOnPage (number)\""
data Header = Header { sessionId :: Maybe String
                     , page :: Maybe String
                     , latency :: Maybe Double
                     , timeOnPage :: Maybe Double } deriving Show
                     
-- Have generated from Header somehow
data HeaderStats = HeaderStats { 
                       sessionIdStats :: ColumnStat
                     , pageStats :: ColumnStat
                     , latencyStats :: ColumnStat
                     , timeOnPageStats :: ColumnStat } deriving Show
                     



defaultNumeric = (Numeric 0 0 0 0 0)
defaultTextual = (Textual 0 0 0 0 0)


updateColumnStatsUnsafeDouble :: ColumnStat -> Double -> ColumnStat
updateColumnStatsUnsafeDouble (Textual _ _ _ _ _) _ = defaultTextual
-- TODO Have min and max take first value if never used before
updateColumnStatsUnsafeDouble oldStats@(Numeric count nullCount minVal maxVal average) val = 
    (Numeric (count + 1) nullCount (min minVal val) (max maxVal val) (updateAverage count average val))


updateColumnStatsUnsafeString :: ColumnStat -> String -> ColumnStat
updateColumnStatsUnsafeString oldStats@(Textual count nullCount shortCount longCount averageLen) val =
    (Textual (count + 1) nullCount (shortCount + 1) (longCount + 1) (updateAverage count averageLen (fromIntegral (length val))))
updateColumnStatsUnsafeString (Numeric _ _ _ _ _) _ = defaultNumeric


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
updateStats :: HeaderStats -> Header -> HeaderStats
-- TODO Remove hardcode of header size
updateStats oldStats@(HeaderStats colA colB colC colD) columnValues@(Header a b c d) = (HeaderStats (updateColumnStatsSafeString colA a) (updateColumnStatsSafeString colB b) (updateColumnStatsSafeDouble colC c) (updateColumnStatsSafeDouble colD d))


-- Converts parsed line to Header type
-- TODO Check to see if this deals with Nothings well
toHeader :: [Maybe String] -> Header
toHeader (a:b:c:d:_) = (Header a b (fmap read c :: Maybe Double) (fmap read d :: Maybe Double))
toHeader _ = (Header (Just "") (Just "") (Just 0) (Just 0))


-- Converts the line into the delimited form
parseMessage :: String -> [Maybe String]
parseMessage message = map (\x -> if x == "" then Nothing else Just x) $ splitOn "," message


main = do
    -- One stat for each column
    let initialStats = (HeaderStats defaultTextual defaultTextual defaultNumeric defaultNumeric)
    -- One message for each column
    let messages = map (toHeader . parseMessage) ["TEST,DSA,1.0,2.1", "AwesomeAnswer,Sup bro,321.9,321.34", "cool story, dhsuadhsua, 5.2, 6.9"]
    print $ foldl updateStats initialStats messages
