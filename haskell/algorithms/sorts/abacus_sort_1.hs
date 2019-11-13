import Control.Exception (assert)

{-
 - Increment first 'n' elements in a given list by 1
 -
 - increment_first 5 [1..10] == [2,3,4,5,6,6,7,8,9,10]
 -}
increment_first :: Int -> [Int] -> [Int]
increment_first n array = map (\x -> x+1) (take n array) ++ drop n array

{-
 - count number of elements in lst that are greater than i
 - count_greater 3 [2,6,1,4,3] == 3
 -}
count_greater :: Int -> [Int] -> Int
count_greater i lst = length $ filter (>i) lst 


{-
 - Iterate down to 0 from n
 - At each iteration, count number of elements in input list that are greater than current iteration/row i
 - Increment as many elements in the temporary list from the left, as the count from previous step.
 -}
abacus_sort' :: [Int] -> [Int] -> Int -> [Int]
abacus_sort' lst temp n 
    | (n<0) = temp -- return temp when iteration count hits 0
    | otherwise = abacus_sort' lst (increment_first (count_greater n lst) temp) (n-1)


{-
 - abacus sort: Takes a list and returns a sorted list in ascending order
 -    This version cannot handle negative numbers
 -}
abacus_sort :: [Int] -> [Int]
abacus_sort lst =
    reverse $ abacus_sort' lst temp (maximum lst - 1)
    where
        temp = replicate (length lst) 0



main = do
    -- Test increment_first
    -- NOTE: return  statement doesnt seem to raise an exception when assert fails
    -- return $ assert ((increment_first 3 [2,6,1,4,3]) == [3,7,2,4,5]) ""
    putStr $ assert ((increment_first 3 [2,6,1,4,3]) == [3,7,2,4,3]) ""
    putStr $ assert ((increment_first 4 [0,0,0,0,0]) == [1,1,1,1,0]) ""
    putStr $ assert ((increment_first 5 [1..10]) == [2,3,4,5,6,6,7,8,9,10]) ""
    putStr $ assert ((increment_first (-1) [0,0,0,0,0]) == [0,0,0,0,0]) ""

    -- Test count_greater
    putStr $ assert ((count_greater 3 [2,6,1,4,3]) == 2) ""
    putStr $ assert ((count_greater 3 [0,0,0,1,2]) == 0) ""
    putStr $ assert ((count_greater 1 [2,6,1,4,3]) == 4) ""

    -- Test the complete sort function
    putStr $ assert ((abacus_sort [2,6,1,4,3]) == [1,2,3,4,6]) ""
    putStr $ assert ((abacus_sort $ [10,9..5]++[1..4]) == [1..10]) ""
    putStr $ assert ((abacus_sort [2,5,1,4]) == [1,2,4,5]) ""
    putStr $ assert ((abacus_sort [1, 2, 4, 2, 5, 3, 2, 3, 4]) == [1,2,2,2,3,3,4,4,5]) ""
    putStr $ assert ((abacus_sort [5, 3, 1, 7, 4, 1, 1, 20]) == [1, 1, 1, 3, 4, 5, 7, 20]) ""

