multipli :: Int -> Int -> Int
multipli m n = length([m `mod` n : multipli (m `div` n) n)
