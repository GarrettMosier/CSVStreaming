import Data.List.Split
-- Use record instead
-- Dynamically generate record from generator exe
data Stats = Textual Int Int Double Double Double | Numeric Int Int Int Int Double deriving Show

updateColumnStatsUnsafe :: Stats -> String -> Stats
updateColumnStatsUnsafe oldStats@(Textual count nullCount shortCount longCount averageLen) msg = 
    (Textual (count + 1) nullCount (shortCount + 1) (longCount + 1) (updateAverage count averageLen (length msg))) 
updateColumnStatsUnsafe oldStats@(Numeric count nullCount min max average) msg = oldStats

updateColumnStatsSafe :: Stats -> Maybe String -> Stats 
updateColumnStatsSafe oldStats@(Textual count nullCount shortCount longCount averageLen) Nothing = 
    (Textual count (nullCount + 1) shortCount longCount averageLen) 
updateColumnStatsSafe oldStats@(Numeric count nullCount min max average) Nothing = oldStats
updateColumnStatsSafe oldStats (Just val) = updateColumnStatsUnsafe oldStats val

updateAverage :: Int -> Double -> Int -> Double 
updateAverage count oldAverage updateVal = (oldAverage * fromIntegral count + fromIntegral updateVal) / fromIntegral (count + 1)

updateStats :: [Stats] -> [Maybe String] -> [Stats]
updateStats oldStats columnValues = zipWith updateColumnStatsSafe oldStats columnValues

main = do
    -- One stat for each column
    let testStats = [(Textual 0 0 0 0 0), (Numeric 0 0 0 0 0)]
    -- One message for each column
    let messages = [map (\x -> if x == "" then Nothing else Just x) $ splitOn "," "TEST,DSA,2d,,"]
    print $ foldl updateStats testStats messages 
