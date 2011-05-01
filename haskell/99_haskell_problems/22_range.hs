{- Create a list containing all integers within a given range. -}

range' :: Int -> Int -> [Int] -> [Int]
range' x y lst =
	if (x > y)
		then lst
		else range' x (y-1) (y:lst)

range :: Int -> Int -> [Int]
range x y = range' x y []
	
