-- Write a function that takes a list of pairs of length at least 2 and returns the first component of the second element in the list. So, when provided with [(5,’b’),(1,’c’),(6,’a’)], it will return 1.

fstOfSecTuple :: [(Int, Char)] -> Int
fstOfSecTuple listOfPairs = fst $ head $ tail listOfPairs

