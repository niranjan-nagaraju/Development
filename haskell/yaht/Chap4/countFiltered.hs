module Count
	where

import IO

count1 :: (a->Bool) -> [a] -> Int
count1 f list = length (filter f list)

count2 :: (a->Bool) -> [a] -> Int
count2 f list = foldr (\x c -> if f x then c+1 else c) 0 list

count3 :: (a->Bool) -> [a] -> Int
count3 f list = foldl (\c x -> if f x then c+1 else c) 0 list
