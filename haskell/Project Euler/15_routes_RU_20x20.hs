import IO

factorial :: Integer -> Integer
factorial n = product [1..n]

main = do
	print $ (factorial 40) `div`  ((factorial 20) * (factorial 20))

{- 137846528820 -}
