-- Author: Garrett Mosier
-- Purpose: Calculate statistics for a CSV file / stream

{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

import Data.List.Split
import Text.Read


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

newtype Count = Count Int deriving Show
newtype NullCount = NullCount Int deriving Show
newtype ShortCount = ShortCount Double deriving Show
newtype LongCount = LongCount Double deriving Show
newtype MinVal = MinVal Double deriving Show
newtype MaxVal = MaxVal Double deriving Show
newtype AverageVal = AverageVal Double deriving Show
newtype AverageLen = AverageLen Double deriving Show 

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


defaultNumeric = Numeric (Count 0) (NullCount 0) (MinVal 0) (MaxVal 0) (AverageVal 0)
defaultTextual = Textual (Count 0) (NullCount 0) (ShortCount 0) (LongCount 0) (AverageLen 0)


-- TODO Have min and max take first value if never used before
updateColumnStatsUnsafeDouble :: ColumnStat NumericKind -> Double -> ColumnStat NumericKind
updateColumnStatsUnsafeDouble
  (Numeric count@(Count c) nullCount (MinVal minVal) (MaxVal maxVal) average) val
  = (Numeric (Count $ c + 1) nullCount (MinVal (min minVal val)) (MaxVal (max maxVal val))
       (updateAverage count average val))


-- Calculates the average value
updateAverage :: Count -> AverageVal -> Double -> AverageVal
updateAverage (Count count) (AverageVal oldAverage) updateVal =
  AverageVal $ (oldAverage * fromIntegral count + updateVal) / (fromIntegral (count + 1))

-- TODO Find cleaner way to do this
toAverageLen :: AverageVal -> AverageLen
toAverageLen (AverageVal a) = (AverageLen a)

updateColumnStatsUnsafeString :: ColumnStat TextualKind -> String -> ColumnStat TextualKind
updateColumnStatsUnsafeString
  (Textual count@(Count c) nullCount (ShortCount shortCount) (LongCount longCount) (AverageLen averageLen)) val =
    (Textual (Count $ c + 1) nullCount (ShortCount $ shortCount + 1) (LongCount $ longCount + 1) (toAverageLen (updateAverage count (AverageVal averageLen) (fromIntegral (length val)))))


-- Finds the stats for all columns given the new line
updateColumnStatsSafeDouble :: ColumnStat NumericKind-> Maybe Double -> ColumnStat NumericKind
updateColumnStatsSafeDouble (Numeric count (NullCount nc) minVal maxVal average) Nothing =
    (Numeric count (NullCount $ nc + 1) minVal maxVal average)
updateColumnStatsSafeDouble oldStats (Just val) = updateColumnStatsUnsafeDouble oldStats val


updateColumnStatsSafeString :: ColumnStat TextualKind-> Maybe String -> ColumnStat TextualKind
updateColumnStatsSafeString (Textual count (NullCount nullCount) shortCount longCount averageLen) Nothing =
    (Textual count (NullCount $ nullCount + 1) shortCount longCount averageLen)
updateColumnStatsSafeString oldStats (Just val) = updateColumnStatsUnsafeString oldStats val


-- Finds the new stats for the file with the new line
updateStats :: HeaderStats -> Header -> HeaderStats
updateStats (HeaderStats colA colB colC colD) (Header a b c d) = (HeaderStats (updateColumnStatsSafeString colA a) (updateColumnStatsSafeString colB b) (updateColumnStatsSafeDouble colC c) (updateColumnStatsSafeDouble colD d))


toMaybeDouble :: Maybe String -> Maybe Double
toMaybeDouble (Just x) = readMaybe x 
toMaybeDouble Nothing = Nothing

-- Converts parsed line to Header type
toHeader :: [Maybe String] -> Header
toHeader [a, b, c, d] = (Header a b (toMaybeDouble c) (toMaybeDouble d))
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
