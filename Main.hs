-- Use record instead
-- Dynamically generate record from generator exe
data Stats = Textual Int Int Double Double Double | Numeric Int Int Int Int Double deriving Show

updateStats :: Stats -> [Char] -> Stats
updateStats oldStats@(Textual a b c d e) msg = oldStats
updateStats oldStats@(Numeric a b c d e) msg = oldStats

main = do
    print "TEST"
    let testText = (Textual 0 0 0 0 0)
    let testNumeric = (Numeric 0 0 0 0 0)
    print $ updateStats testText "DSA"
    print $ updateStats testNumeric "DSA"
