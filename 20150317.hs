vendas :: Int -> Int -> Int
vendas n m = mod n m

funcao :: Int -> Int -> Int -> Int
funcao s n m| (n == 0) && (vendas 0 m == s)	= 1
	    | (n == 0) && (vendas 0 m /= s) 	= 0
	    | vendas n m == s 			= funcao s (n-1) m + 1
	    | vendas n m /= s 			= funcao s (n-1) m