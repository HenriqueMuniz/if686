double :: [Int] -> [Int]
double l | tail l == [] = [(2 * (head l))]
         | otherwise    = (2 * (head l)) : double (tail l)

member :: [Int] -> Int -> Bool
member l n | l == []     = False
           | head l == n = True
           | otherwise   = member (tail l) n

digits :: String -> String
digits l | l == []                       = []
         | head l >= '0' && head l <= '9'= head l : digits (tail l)
         | otherwise                     = digits (tail l)

sumPairs :: [Int] -> [Int] -> [Int]
sumPairs l m | l == [] && m == [] = []
             | l == []         =            head m : sumPairs []       (tail m)
             | m == []         =            head l : sumPairs (tail l) []
             | otherwise       = (head l + head m) : sumPairs (tail l) (tail m)

quicksort :: [Int] -> [Int]
quicksort l | l == []   = []
            | otherwise = (quicksort (isLow (tail l) (head l))) ++ [head l] ++ (quicksort (isHigh (tail l) (head l)))

isLow :: [Int] -> Int -> [Int]
isLow l n | l == []    = []
          | head l < n = (head l) : isLow (tail l) n
          | otherwise  = isLow (tail l) n

isHigh :: [Int] -> Int -> [Int]
isHigh l n| l == []    = []
          | head l > n = (head l) : isHigh (tail l) n
          | otherwise  = isHigh (tail l) n
