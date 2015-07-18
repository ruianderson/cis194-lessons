module Golf where

skips :: [a] ->[[a]]
skips [] = []
skips [x] = [[x]]
skips l@(_:_:xs) = [l] ++ [everySecond l] ++ toList xs

everySecond :: [a] -> [a]
everySecond [] = []
everySecond [x] = []
everySecond (_:x:xs) = [x] ++ everySecond xs

toList :: [a] -> [[a]]
toList [] = []
toList [x] = [[x]]
toList (x:xs) = [[x]] ++ toList xs

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x1:x2:[]) = []
localMaxima (x1:x2:xs)
  | x2 > x1 && x2 > head xs = [x2] ++ localMaxima (x2:xs)
  | otherwise               = localMaxima (x2:xs)

histogram :: [Integer] -> String
histogram xs = unlines ([row | x <- [i, i - 1..1], let row = stars x f] ++ e ++ n)
  where f = frequences xs
        i = interactions f
        e = ["=========="]
        n = [['0'..'9']]

frequences :: [Integer] -> [(Integer, Integer)]
frequences xs = [(x, freq) | x <- [0..9], let freq = fromIntegral . length $ filter (== x) xs]

interactions :: [(Integer, Integer)] -> Integer
interactions xs = maximum $ map snd xs

stars :: Integer -> [(Integer, Integer)] -> [Char]
stars i [] = []
stars i (x:xs)
  | frequency > 0 && frequency >= i = "*" ++ stars i xs
  | otherwise                       = " " ++ stars i xs
  where frequency = snd x
