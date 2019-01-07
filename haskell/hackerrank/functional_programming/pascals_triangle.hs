{-
Print a pascal triangle on length k

1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
....


input:
4

Output:
1  
1 1  
1 2 1  
1 3 3 1  
-}

{- 
 - Calculate next row for a pascal's triangle based on current one and return it.
 -
 - Prelude> next [1,2,1]
 - [1,3,3,1]
 -}
next ::  [Int] -> [Int]
next row = [1] ++ map (\x -> (row!!x) + (row!!(x+1))) [0 .. (length row)-2] ++ [1]


{- 
 - Add next row to the pascal's triangle calculated based on the last row currently in the triangle 
 - and return the updated pascal's triangle
 -
 - Prelude> add_next_row [[1]]
 - [[1],[1,1]]
 - Prelude> add_next_row [[1],[1,1]]
 - [[1],[1,1],[1,2,1]]
 -}
add_next_row :: [[Int]] -> [[Int]]
add_next_row triangle  = triangle ++ [next $ last triangle]


{- 
 - print a list as space-separated items 
 -
 - Prelude> print_list [1,2,3]
 - 1 2 3
 -}
print_list row = putStrLn $ foldr (\x acc -> show x ++ " " ++ acc) "" row


{- 
 - Return a 2D list containing the pascal triangle of length n 
 -
 - Prelude> pascal_triangle 1
 - [[1]]
 -
 - Prelude> pascal_triangle 3
 - [[1],[1,1],[1,2,1]]
 - 
 - Prelude> pascal_triangle 4
 - [[1],[1,1],[1,2,1],[1,3,3,1]]
 -} 
pascal_triangle :: Int -> [[Int]]
pascal_triangle 0 = []
pascal_triangle 1 = [[1]]
pascal_triangle n = foldr (\x triangle -> add_next_row triangle) [[1]] [2..n]


{-
 - *Main> main
 - 4
 - 1 
 - 1 1 
 - 1 2 1 
 - 1 3 3 1 
 -}
main = do
	n <- readLn :: IO Int
	{-
	 - Prelude> mapM_ print_list [[1,2,3], [1,2]]
	 - 1 2 3 
	 - 1 2 
	 -}
	mapM_ print_list (pascal_triangle n)

