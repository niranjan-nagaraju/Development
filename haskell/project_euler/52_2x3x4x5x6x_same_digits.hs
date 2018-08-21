import IO
import List

isCandidate num =
	let xList = map (\x -> num * x) [2 .. 6]
	in
	(length (nub (map (\x -> (sort $ show x)) xList)) == 1)

getCandidate num =
	if (isCandidate num)
		then num
		else getCandidate (num+1)

main = do
	print $ getCandidate 1 
	

{- 142857 -}
