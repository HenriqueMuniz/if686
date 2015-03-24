------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Exercicios de Sorts
merge :: [Int] -> [Int] -> [Int]
merge [] []      = []
merge l  []      = l
merge [] l       = l
merge (a:as) (b:bs) | a <= b     = a : (merge as     (b:bs))
                    | otherwise  = b : (merge (a:as) bs)

split :: [Int] -> ([Int], [Int])
split []        = ([],[])
split (a:[])	= ([a], [])
split (a:b:[])	= ([a], [b])
split (a:b:as) 	= (a : fst (split as), b : snd (split as))


mergeSort :: [Int] -> [Int]
mergeSort []     = []
mergeSort (a:[]) = [a]
mergeSort l      = merge (mergeSort (fst (split l))) (mergeSort (snd (split l)))


quicksort :: [Int] -> [Int]
quicksort l | l == []   = []
            | otherwise = (quicksort (isLow (tail l) (head l))) ++ [head l] ++ (quicksort (isHigh (tail l) (head l)))

isLow :: [Int] -> Int -> [Int]
isLow l n | l == []    = []
          | head l <= n = (head l) : isLow (tail l) n
          | otherwise  = isLow (tail l) n

isHigh :: [Int] -> Int -> [Int]
isHigh l n| l == []    = []
          | head l > n = (head l) : isHigh (tail l) n
          | otherwise  = isHigh (tail l) n

len :: [Int] -> Int
len [] = 0
len l  = 1 + len (tail l)

findEl :: [Int] -> Int -> Int
findEl [] _ = -2000000000000000000
findEl l  1 = head l
findEl l n  = findEl (tail l) (n-1)


findElPermuta :: [Int] -> Int -> ([Int],[Int])
findElPermuta [] _ = ([],[])
findElPermuta l  1 = ([], tail l)
findElPermuta l n  = (head l : fst (findElPermuta (tail l) (n-1)), [] ++ snd (findElPermuta (tail l) (n-1)))

permuta :: [Int] -> Int -> Int -> [Int]
permuta [] _ _           = []
permuta l 1 j
             | j > len l = l
	     | j == 1    = l
             | otherwise = (findEl l j) : fst(findElPermuta (tail l) (j -1)) ++ [head l] ++ snd (findElPermuta (tail l) (j-1))
permuta l i j
             | i == j    = l
	     | otherwise = head l : permuta (tail l) (i-1) (j-1)


heapfy :: [Int] -> Int -> [Int]
heapfy [] _ = []
heapfy l  i | ((findEl l  (2*i)      > findEl l (i)) && (findEl l  (2*i)      > findEl l ((2*i) + 1))) = heapfy ((permuta l i  (2*i)))       (2*i)
	    | ((findEl l ((2*i) + 1) > findEl l (i)) && (findEl l ((2*i) + 1) > findEl l  (2*i)))      = heapfy ((permuta l i ((2*i) + 1))) ((2*i) + 1)
            |  (findEl l  (2*i)      > findEl l (i))                                                   = heapfy ((permuta l i  (2*i)))       (2*i)
	    |  (findEl l ((2*i) + 1) > findEl l (i))                                                   = heapfy ((permuta l i ((2*i) + 1))) ((2*i) + 1)
	    | otherwise = l


buildMaxHeap :: [Int] -> Int -> [Int]
buildMaxHeap [] _ = []
buildMaxHeap l  1 = heapfy l 1
buildMaxHeap l  i = buildMaxHeap(heapfy l i) (i-1)

heapSort :: [Int] -> [Int]
heapSort []     = []
heapSort (a:[]) = [a]
heapSort l  =  heapSort(tail (buildMaxHeap l (len l))) ++ [head (buildMaxHeap l (len l))]



------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Exercicios da aula
minimo :: Int -> Int -> Int -> Int
minimo a b c 
             | ((a < b) && (a < c)) = a
             | ((b < a) && (b < c)) = b
             | ((c < a) && (c < b)) = c

maximo :: Int -> Int -> Int -> Int
maximo a b c 
             | ((a > b) && (a > c)) = a
             | ((b > a) && (b > c)) = b
             | ((c > a) && (c > b)) = c

menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c = ((minimo a b c), (maximo a b c))


ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a, b, c) = ((fst (menorMaior a b c)), x, (snd (menorMaior a b c))) where x = a + b + c - (fst (menorMaior a b c)) - (snd (menorMaior a b c))


