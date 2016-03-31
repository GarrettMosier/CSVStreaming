-- Author: Garrett Mosier
-- Purpose: Calculate statistics for a CSV file / stream

{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

import Data.List.Split
import Text.Read
import Data.Csv.Streaming
import qualified Data.ByteString.Lazy as BL

-- TODO Use quickCheck for tests
-- TODO Use lens library to get into records and data types

data ColumnStat kind where
  Textual :: Count -> NullCount -> ShortCount -> LongCount -> AverageLen -> ColumnStat TextualKind
  Numeric :: Count -> NullCount -> MinVal -> MaxVal -> AverageVal -> ColumnStat NumericKind
deriving instance Show (ColumnStat a)

type Textual = ColumnStat TextualKind
type Numeric = ColumnStat NumericKind

data TextualKind
data NumericKind

type Count = Int 
type NullCount = Int 
type ShortCount = Double 
type LongCount = Double 
type MinVal = Double
type MaxVal = Double 
type AverageVal = Double 
type AverageLen = Double  

-- TODO Dynamically generate record from generator exe
--exampleHeader = "\"sessionId (text)\",\"page (text)\",\"latency (number)\",\"timeOnPage (number)\""
data Header = Header { sessionId :: Maybe String
                     , page :: Maybe String
                     , latency :: Maybe Double
                     , timeOnPage :: Maybe Double } deriving Show

-- TODO Have generated from Header somehow
data HeaderStats = HeaderStats {
                       sessionIdStats :: Textual
                     , pageStats :: Textual
                     , latencyStats :: Numeric
                     , timeOnPageStats :: Numeric } deriving Show


defaultNumeric = Numeric 0 0 0 0 0
defaultTextual = Textual 0 0 0 0 0


-- TODO Have min and max take first value if never used before
updateColumnStatsUnsafeDouble :: ColumnStat NumericKind -> Double -> ColumnStat NumericKind
updateColumnStatsUnsafeDouble (Numeric count nullCount minVal maxVal average) val
  = (Numeric (count + 1) nullCount ((min minVal val)) ((max maxVal val)) (updateAverage count average val))


-- Calculates the average value
updateAverage :: Count -> AverageVal -> Double -> AverageVal
updateAverage count oldAverage updateVal = (oldAverage * fromIntegral count + updateVal) / (fromIntegral (count + 1))


-- TODO Find cleaner way to do this
toAverageLen :: AverageVal -> AverageLen
toAverageLen (a) = (a)

updateColumnStatsUnsafeString :: ColumnStat TextualKind -> String -> ColumnStat TextualKind
updateColumnStatsUnsafeString (Textual count nullCount shortCount longCount averageLen) val =
    (Textual (count + 1) nullCount (shortCount + 1) (longCount + 1) (toAverageLen (updateAverage count averageLen (fromIntegral (length val)))))


-- Finds the stats for all columns given the new line
updateColumnStatsSafeDouble :: ColumnStat NumericKind-> Maybe Double -> ColumnStat NumericKind
updateColumnStatsSafeDouble (Numeric count (nullCount) minVal maxVal average) Nothing =
    (Numeric count (nullCount + 1) minVal maxVal average)
updateColumnStatsSafeDouble oldStats (Just val) = updateColumnStatsUnsafeDouble oldStats val


updateColumnStatsSafeString :: ColumnStat TextualKind-> Maybe String -> ColumnStat TextualKind
updateColumnStatsSafeString (Textual count nullCount shortCount longCount averageLen) Nothing = 
    (Textual count (nullCount + 1) shortCount longCount averageLen)
updateColumnStatsSafeString oldStats (Just val) = updateColumnStatsUnsafeString oldStats val


-- Finds the new stats for the file with the new line
updateStats :: HeaderStats -> Maybe Header -> HeaderStats
updateStats (HeaderStats colA colB colC colD) (Just (Header a b c d)) = (HeaderStats (updateColumnStatsSafeString colA a) (updateColumnStatsSafeString colB b) (updateColumnStatsSafeDouble colC c) (updateColumnStatsSafeDouble colD d))
updateStats oldStats Nothing = oldStats

toMaybeDouble :: Maybe String -> Maybe Double
toMaybeDouble x = x >>= readMaybe 


-- Converts parsed line to Header type
toHeader :: [Maybe String] -> Maybe Header
toHeader [a, b, c, d] = Just (Header a b (toMaybeDouble c) (toMaybeDouble d))
toHeader _ = Nothing 


-- Converts the line into the delimited form
parseMessage :: String -> [Maybe String]
parseMessage message = map (\x -> if x == "" then Nothing else Just x) $ splitOn "," message


main = do
    -- One stat for each column
    let initialStats = (HeaderStats defaultTextual defaultTextual defaultNumeric defaultNumeric)
    csvData <- BL.readFile "testCSV.txt"
    print csvData
    -- One message for each column
    let messages = map (toHeader . parseMessage) ["TEST,DSA,1.0,2.1", "AwesomeAnswer,Sup bro,321.9,321.34", "cool story, dhsuadhsua, 5.2, 6.9"]
    let finalStats = foldl updateStats initialStats messages
    print finalStats
    
