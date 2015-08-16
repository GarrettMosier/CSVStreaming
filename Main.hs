-- Use record instead
-- Dynamically generate record from generator exe
data Stats = Textual Int Int Double Double Double | Numeric Int Int Int Int Double deriving Show

updateStats :: Stats -> [Char] -> Stats
updateStats oldStats@(Textual count nullCount shortCount longCount averageLen) msg = 
    (Textual (count + 1) (nullCount + 1) (shortCount + 1) (longCount + 1) (updateAverage count averageLen (length msg))) 
updateStats oldStats@(Numeric count nullCount min max average) msg = oldStats

updateAverage :: Int -> Double -> Int -> Double 
updateAverage count oldAverage updateVal = (oldAverage * fromIntegral count + fromIntegral updateVal) / fromIntegral (count + 1)

main = do
    let testText = (Textual 0 0 0 0 0)
    let testNumeric = (Numeric 0 0 0 0 0)
    let messages = ["TEST", "DSA", "2d"]
    print $ foldl updateStats testText messages 
